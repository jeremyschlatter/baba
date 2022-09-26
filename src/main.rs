use ggez::*;

struct State {
    dt: std::time::Duration,
    n: u32,
}

impl ggez::event::EventHandler<GameError> for State {
    fn update(&mut self, ctx: &mut Context) -> GameResult {
        self.dt = ctx.time.delta();
        self.n += 1;
        Ok(())
    }
    fn draw(&mut self, ctx: &mut Context) -> GameResult {
        if self.n % 10 == 0 {
            println!("Hello ggez! dt = {}ms", self.dt.as_millis());
        }
        Ok(())
    }
}

fn main() {
    let state = State {
        dt: std::time::Duration::new(0, 0),
        n: 0,
    };
    let c = conf::Conf::new();
    let (ctx, event_loop) = ContextBuilder::new("hello_ggez", "Jeremy")
        .default_conf(c)
        .build()
        .unwrap();

    event::run(ctx, event_loop, state);
}
