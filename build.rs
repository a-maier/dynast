use vergen::EmitBuilder;

fn main() {
    // If this is not run inside a git repository we get an error.
    // This happens when installing the crate via cargo.
    // As a quick fix, we just ignore it.
    let _ = EmitBuilder::builder()
        .git_sha(true)
        .emit();
}
