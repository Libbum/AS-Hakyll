---
title: Code Coverage in Rust
byline: Productivity tools you employ to make yourself more efficient that actually take up most of your productive time.
tags: Rust
---

One side project that has been sitting by the wayside since completing my PhD has been to polish up and publish [Wa*f*er](https://github.com/Libbum/Wafer), my fancy 3D wavefunction solver.
For reasons, one of which is mostly [structured procratstination](http://www.chronicle.com/article/How-to-ProcrastinateStill/93959), this polishing ended up being a port/re-write in Rust.
Delving into the Rust ecosystem has been fun and if you check Wa*f*er's commit history you'll notice large rewrites again and again as I basically play around with things; having no real development schedule or design restrictions.

It may seem a little counter intuitive then, that I'm also trying at the same time to learn all the rigid structures of software development.
Proper documentation, testing, error handling, data structures *etc*. <!--BLURB--> &nbsp;
The Rust tooling makes much of this a breeze.
For example, you write comments above your functions in markdown, call `cargo doc` and a complete document is compiled for you.

Rust is actively evolving though, and hiccups do occur -- sometimes more that you'd like.
Staying with the documentation example, if you want to document all your routines as more of a developers document, the command to do so is simply

```
cargo rustdoc --open -- --no-defaults --passes collapse-docs --passes unindent-comments --passes strip-priv-imports
```

and obviously rolls off the keyboard just as effortlessly as `cargo doc`.
[Fixes](https://github.com/rust-lang/cargo/issues/1520) are in the works for this oversight, and it's really not too much more than a quirk that's fairly obvious to find a solution for.
Usually that comes from a well written article on some well known and respected Rust blogger's site.

Recently, [an excellent write up](https://medium.com/@Razican/continuous-integration-and-code-coverage-report-for-a-rust-project-5dfd4d68fbe5) by Razican on getting Rust to play with [Travis](https://travis-ci.org/) enabled me to setup a seemless testing environment, with developer documentation instantly updated on every commit ([here](https://libbum.github.io/Wafer/wafer/index.html) if you want to check it out).

Formal, structured testing is somewhat new to me.
Previously testing was mostly spitting debug data to stdout and making sure numbers matched up to some expected output.
Travis is a tool that other open source projects have had enabled, so it's existence is known to me, and previously even managed two [add some tests](https://github.com/JuliaEditorSupport/atom-language-julia/commit/4f249a64ffdb8133e7453cbc28f2573afd85016a) that did a good job of failing for a while before getting it right.

The idea of code coverage however is something I'd never heard of before Razican's post.
So, to check it out I chucked in the extra configuration steps to my `.travis.yml` and set up a [Codecov](https://codecov.io/) account.
To my surprise, the five or so extremely minimal tests I'd written gave me an impressive 96% coverage!

Something didn't add up.

Pulling up the [raw logs](https://codecov.s3.amazonaws.com/v4/raw/2017-06-05/D2380B539060047E6F2FB8FE6AEBA933/c495f03ffbdf8a845a7e0bfcf6e0ab0ef5d4750b/2a592f0c-9546-46d8-8951-5ecd48c3ccb0.txt), it was clear that `total_lines` here

```json
{"file": "/home/travis/build/Libbum/Wafer/src/grid.rs", "percent_covered": "100.00", "covered_lines": "35", "total_lines": "35"}
```

needs to be a little larger.
That file was 722 lines long at the time.

Codecov support replied to me extremely quickly, although they couldn't help solve the issue: The raw logs are what they match on, so it's the rust compiler that's not producing them correctly.

Here's the point where I'd expect again to say that there's a bit of a quirk with a fairly well documented solution again, but this time none was to be found.
Not that there wasn't a solution, but there just wasn't an easily explained blog post about it.
Alas, the burden has therefore fallen squarely on my lap.





Hi Steve!

I managed to identify the reasoning and useful workaround.

For future reference, this is an excellent description of *why* it happens:

https://internals.rust-lang.org/t/disabling-gc-sections-when-test-is-specified/2163


A solution has been implemented, but not widely talked about in the rust community:

https://github.com/rust-lang/rust/pull/31368


So my fix is to run `cargo test` as `RUSTFLAGS=-Clink-dead-code cargo test` in my .travis.yml:

https://github.com/Libbum/Wafer/commit/32161111cb16d579506dd1568228c08471d8ea94
And now check out my beautiful 9%!
https://codecov.io/gh/Libbum/Wafer


Hope that helps others using your service when they come upon this issue.

Thanks!


