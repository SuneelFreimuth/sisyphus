use std::fs::File;
use std::io::Read;
use std::sync;
use std::sync::Arc;
use std::{env, fmt::Display};

mod util;
use util::*;

mod lex;
use lex::{lex, Operator, Token};

mod parse;
use parse::{parse, PostfixExpr};
use tokio::sync::Mutex;

use serde::Deserialize;

const FLOAT_TOLERANCE: f64 = 1e-5;

use serenity::{
    async_trait,
    model::{
        channel::Message,
        gateway::Ready,
        prelude::{ChannelId, UserId},
        user::User,
    },
    prelude::*,
};

#[tokio::main]
async fn main() {
    let config: Config = {
        let args: Vec<String> = env::args().collect();
        let config_path = args
            .get(1)
            .map(String::clone)
            .unwrap_or(String::from("config.toml"));
        let mut file = File::open(config_path).expect("could not open config.toml");
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        toml::from_str(contents.as_str()).unwrap()
    };

    let admins: Option<Vec<Admin>> = config
        .admins
        .map(|admins| admins.into_iter().map(Admin::from_string).collect());

    let intents = GatewayIntents::GUILD_MESSAGES
        | GatewayIntents::DIRECT_MESSAGES
        | GatewayIntents::MESSAGE_CONTENT;

    if let Some(ref admins) = admins {
        println!("Starting bot with administrators:");
        for admin in admins {
            println!("  {}", admin.to_string());
        }
    } else {
        println!("Starting bot...")
    }

    let mut client = Client::builder(&config.token, intents)
        .event_handler(Bot::new(admins.unwrap_or_else(Vec::new)))
        .await
        .expect("Error creating client");

    if let Err(why) = client.start().await {
        println!("Client error: {:?}", why);
    }
}

struct Bot {
    // Guarded uses a std::sync::Mutex; The sigil does not need to be locked
    // across `.await` points, so this is fine.
    sigil: Guarded<String>,
    watching: Mutex<Option<CountingChannel>>,
    admins: Vec<Admin>,
}

struct CountingChannel {
    id: ChannelId,
    next_count: u32,
    last_counter: Option<UserId>,
}

impl CountingChannel {
    pub fn is_last_counter(&self, user: &User) -> bool {
        self.last_counter.is_some_and(|last| last == user.id)
    }
}

enum BotMessage {
    Command(),
}

enum Command<'a> {
    Help,
    Stat,
    Eval(&'a str),
    Watch(&'a str),
    NoWatch,
    SetSigil(&'a str),
}

impl Bot {
    fn new(admins: Vec<Admin>) -> Self {
        Self {
            sigil: Guarded::new("$".into()),
            watching: Mutex::new(None),
            admins,
        }
    }

    fn with_sigil(&self, name: &str) -> String {
        format!("{}{}", self.sigil.get(), name)
    }

    fn help_text(&self) -> String {
        format!(
            "# [Sisyphus](https://github.com/SuneelFreimuth/sisyphus)
To +∞ and beyond!
### Commands
- `{s}help`  For more detailed documentation, see the README.
- `{s}stat`  View bot status.
- `{s}eval <expr...>`  Evaluate everything following the `$eval` command as an arithmetic expression.
Admin-only:
- `{s}watch <channel>`  Watch the mentioned channel.
- `{s}nowatch`  Stop watching the counting channel.
- `{s}setsigil <sigil>`  Set command sigil.",
            s=self.sigil.get()
        )
    }

    fn is_admin(&self, user: &User) -> bool {
        self.admins
            .iter()
            .any(|admin| user.name == admin.name && user.discriminator == admin.discriminator)
    }

    async fn send(&self, ctx: Context, chan: ChannelId, msg: impl Display) {
        if let Err(why) = chan.say(&ctx.http, msg).await {
            println!("Error sending message: {:?}", why);
        }
    }
}

#[async_trait]
impl EventHandler for Bot {
    async fn message(&self, ctx: Context, msg: Message) {
        if msg.is_own(&ctx.cache) || msg.author.bot {
            return;
        }

        let content = msg.content.trim();

        // TODO: Lex properly
        if content == self.with_sigil("help") {
            self.send(ctx, msg.channel_id, self.help_text()).await;
        } else if content.starts_with(&self.with_sigil("eval")) {
            let raw_expr = content.strip_prefix(&self.with_sigil("eval")).unwrap();
            if let Ok(tokens) = lex(&raw_expr) {
                let expr = parse(tokens);
                let result = eval_postfix_expr(expr).round() as u32;
                self.send(ctx, msg.channel_id, result.to_string()).await;
            }
        } else if self.is_admin(&msg.author) && content.starts_with(&self.with_sigil("watch")) {
            let raw_channel_id = content
                .strip_prefix(&self.with_sigil("watch"))
                .unwrap()
                .trim()
                .strip_prefix("<#")
                .unwrap()
                .strip_suffix(">")
                .unwrap()
                .parse::<u64>()
                .unwrap();
            println!("{content}, {raw_channel_id}");
            let mut watching = self.watching.lock().await;
            *watching = Some(CountingChannel {
                id: ChannelId(raw_channel_id),
                next_count: 1,
                last_counter: None,
            });
            self.send(
                ctx,
                msg.channel_id,
                format!("Now watching <#{}>", raw_channel_id),
            )
            .await;
        } else if self.is_admin(&msg.author) && content == self.with_sigil("nowatch") {
            let mut watching = self.watching.lock().await;
            if let Some(ref counting_chan) = *watching {
                self.send(
                    ctx,
                    msg.channel_id,
                    format!("No longer watching <#{}>", counting_chan.id),
                )
                .await;
                *watching = None;
            } else {
                self.send(ctx, msg.channel_id, "Not currently watching any channel.")
                    .await;
            }
        } else if content.starts_with(&self.with_sigil("stat")) {
            let watching = self.watching.lock().await;
            if let Some(ref counting_chan) = *watching {
                self.send(
                    ctx,
                    msg.channel_id,
                    format!(
                        "Currently watching <#{}>; next number is {}.",
                        counting_chan.id.to_string(),
                        counting_chan.next_count
                    ),
                )
                .await;
            } else {
                self.send(ctx, msg.channel_id, "Not currently watching any channels.")
                    .await;
            }
        } else if self.is_admin(&msg.author) && content.starts_with(&self.with_sigil("setsigil")) {
            let new_sigil = content
                .strip_prefix(&self.with_sigil("setsigil"))
                .unwrap()
                .trim();
            if new_sigil.len() == 1 {
                self.sigil.set(new_sigil.into());
                self.send(
                    ctx,
                    msg.channel_id,
                    format!("Sigil set to `{}`.", self.sigil.get()),
                )
                .await;
            } else {
                self.send(
                    ctx,
                    msg.channel_id,
                    "Sigil must be a single Unicode character.",
                )
                .await;
            }
        } else {
            let mut watching = self.watching.lock().await;
            if let Some(ref mut counting_chan) = *watching {
                if counting_chan
                    .last_counter
                    .is_some_and(|last| last == msg.author.id)
                {
                    self.send(ctx, msg.channel_id, "Wait for someone else to count.")
                        .await;
                } else if msg.channel_id == counting_chan.id {
                    if let Ok(tokens) = lex(&content) {
                        let expr = parse(tokens);
                        let result = eval_postfix_expr(expr).round() as u32;
                        if result == counting_chan.next_count {
                            msg.react(ctx, '✅').await.unwrap();
                            counting_chan.next_count += 1;
                            counting_chan.last_counter = Some(msg.author.id);
                        } else {
                            self.send(
                                ctx,
                                msg.channel_id,
                                format!(
                                    "It was actually {}. L, ratio, etc.",
                                    counting_chan.next_count
                                ),
                            )
                            .await;
                            counting_chan.next_count = 1;
                        }
                    } else {
                        self.send(ctx, msg.channel_id, "Not a valid infix expression.")
                            .await;
                    }
                }
            }
        }
    }

    async fn ready(&self, _: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
    }
}

#[derive(Deserialize)]
struct Config {
    token: String,
    admins: Option<Vec<String>>,
}

struct Admin {
    name: String,
    // Will always be 0 during the username migration
    discriminator: u16,
}

impl Admin {
    pub fn from_string(name: String) -> Self {
        if let Some(i) = name.find("#") {
            Self {
                name: name[0..i].into(),
                discriminator: name[i + 1..].parse::<u16>().unwrap(),
            }
        } else {
            Self {
                name: name.into(),
                discriminator: 0,
            }
        }
    }

    pub fn to_string(&self) -> String {
        if self.discriminator > 0 {
            format!("{}#{:04}", self.name, self.discriminator)
        } else {
            self.name.clone()
        }
    }
}

pub fn join(mut strs: impl Iterator<Item = String>, sep: &str) -> String {
    let Some(mut result) = strs.next() else {
        return String::new();
    };
    for s in strs {
        result.push_str(sep);
        result.push_str(s.as_str());
    }
    result
}

fn eval_postfix_expr(expr: PostfixExpr) -> f64 {
    let mut stack: Vec<f64> = Vec::new();
    for token in expr.queue.into_iter() {
        match token {
            Token::Number(n) => {
                stack.push(n);
            }
            Token::Operator(op) => {
                assert!(stack.len() >= 2);
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let result = match op {
                    Operator::Plus => a + b,
                    Operator::Minus => a - b,
                    Operator::Star => a * b,
                    Operator::Slash => a / b,
                    Operator::Percent => a % b,
                    Operator::Caret => a.powf(b),
                };
                stack.push(result);
            }
            _ => unreachable!(),
        }
    }
    assert!(stack.len() == 1);
    stack.pop().unwrap()
}

mod tests {
    use super::*;

    #[test]
    fn execute_basic_expressions() {
        assert!(equal_within(
            eval_postfix_expr(PostfixExpr {
                queue: vec![
                    Token::Number(1.),
                    Token::Number(1.),
                    Token::Operator(Operator::Plus),
                ]
            }),
            2.,
            FLOAT_TOLERANCE
        ));
        assert!(equal_within(
            eval_postfix_expr(PostfixExpr {
                queue: vec![
                    Token::Number(1.),
                    Token::Number(2.),
                    Token::Number(3.),
                    Token::Operator(Operator::Star),
                    Token::Operator(Operator::Plus),
                    Token::Number(4.),
                    Token::Operator(Operator::Minus),
                ]
            }),
            3.,
            FLOAT_TOLERANCE
        ));
    }

    #[test]
    fn regressions() {
        let tokens = lex("12      * 5^2 / 5^2").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(12.),
                Token::Operator(Operator::Star),
                Token::Number(5.),
                Token::Operator(Operator::Caret),
                Token::Number(2.),
                Token::Operator(Operator::Slash),
                Token::Number(5.),
                Token::Operator(Operator::Caret),
                Token::Number(2.),
            ]
        );

        let expr = parse(tokens);
        assert_eq!(
            expr,
            PostfixExpr {
                queue: vec![
                    Token::Number(12.),
                    Token::Number(5.),
                    Token::Number(2.),
                    Token::Operator(Operator::Caret),
                    Token::Operator(Operator::Star),
                    Token::Number(5.),
                    Token::Number(2.),
                    Token::Operator(Operator::Caret),
                    Token::Operator(Operator::Slash),
                ]
            }
        );

        assert_eq!(eval_postfix_expr(expr) as i32, 12);
    }
}
