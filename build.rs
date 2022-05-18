use vergen::{vergen, Config, ShaKind};

fn main() {
    let mut cfg = Config::default();
    *cfg.git_mut().sha_kind_mut() = ShaKind::Short;
    // If this is not run inside a git repository we get an error.
    // This happens when installing the crate via cargo.
    // As a quick fix, we just ignore it.
    let _ = vergen(cfg);
}
