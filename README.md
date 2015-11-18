This example for profiling Haskell code was
written by Sampsa "Tuplanolla" Kiiskinen between 2015-11-06 and 2015-11-12.

# The Plan

Generating point set topologies? No.
Solving the maximum subarray sum problem? No.
Computing polygonal area? No.
Finding the total energy of point masses? No.
Running variational Monte Carlo? Yes!

# Project Structure

The `profiling.cabal` file shows how to go from
a slow implementation in `VMC.hs` to
a fast implementation in ten easy steps.
A complete analysis is provided in `VMC.lhs` and
the conclusive implementation is in `QHO.hs`.

# Useful Commands

Modify shell resource limits to avoid freezing your system while profiling.

    $ ulimit -Sv 500000 # 500M

Configure the project for dynamic linking.

    $ cabal configure \
      --disable-executable-profiling --disable-library-profiling \
      --enable-executable-dynamic --enable-shared

Measure performance with GNU Core Utilities or such.

    $ cabal build && for n in $(seq 1 4)
      do echo $n && time dist/build/step-$n/step-$n 100000 3
      done

Read about the runtime system.

    $ xdg-open https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html

Configure the project for profiling.

    $ cabal configure \
      --disable-executable-dynamic --disable-shared \
      --enable-executable-profiling --enable-library-profiling

Measure productivity with GHC RTS.

    $ for n in $(seq 1 6)
      do cabal run step-$n -- 100000 3 +RTS -sstderr
      done

Generate a report.

    $ n=6
    $ cabal run step-$n -- +RTS -p > /dev/null && \
      view step-$n.prof

Generate a colorful plot.

    $ n=6
    $ cabal run step-$n -- +RTS -hy > /dev/null && \
      hp2ps -c step-$n.hp && \
      xdg-open step-$n.ps

Consider choosing another backend.

    $ apt-get install llvm-3.5-dev

Inspect the generated code.

    $ n=9
    $ ghc -O0 -ddump-simpl Step$n.hs > Step$n.ghc &&
      view Step$n.ghc
    $ ghc -O3 -ddump-simpl Step$n.hs > Step$n.ghc &&
      view Step$n.ghc
    $ ghc -O3 -ddump-simpl -dsuppress-all Step$n.hs > Step$n.ghc &&
      view Step$n.ghc
    $ objdump -D Step$n > Step$n.s &&
      view Step$n.s

# Some Results

Run 100000 iterations in 3 dimensions for the first nine steps.

    $ for n in $(seq 1 9) ; do cabal run step-$n -- 100000 3 +RTS -sstderr ; done
    Preprocessing executable 'step-1' for profiling-0.0.0...
    Running step-1...
    1.563643835171424 ± 1.1065032704312414e-3
       2,176,627,728 bytes allocated in the heap
         647,748,056 bytes copied during GC
          90,636,368 bytes maximum residency (13 sample(s))
          14,444,800 bytes maximum slop
                 233 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      4160 colls,     0 par    0.316s   0.316s     0.0001s    0.0019s
      Gen  1        13 colls,     0 par    0.311s   0.312s     0.0240s    0.0884s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.896s  (  0.896s elapsed)
      GC      time    0.627s  (  0.628s elapsed)
      EXIT    time    0.003s  (  0.003s elapsed)
      Total   time    1.529s  (  1.527s elapsed)

      %GC     time      41.0%  (41.1% elapsed)

      Alloc rate    2,428,555,377 bytes per MUT second

      Productivity  59.0% of total user, 59.1% of total elapsed

    Preprocessing executable 'step-2' for profiling-0.0.0...
    Running step-2...
    1.563643835171424 ± 1.1065032704312414e-3
       2,163,027,736 bytes allocated in the heap
         629,384,352 bytes copied during GC
          88,191,584 bytes maximum residency (12 sample(s))
          12,737,816 bytes maximum slop
                 217 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      4106 colls,     0 par    0.317s   0.317s     0.0001s    0.0017s
      Gen  1        12 colls,     0 par    0.291s   0.291s     0.0242s    0.0899s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.913s  (  0.913s elapsed)
      GC      time    0.608s  (  0.608s elapsed)
      EXIT    time    0.003s  (  0.003s elapsed)
      Total   time    1.527s  (  1.524s elapsed)

      %GC     time      39.8%  (39.9% elapsed)

      Alloc rate    2,369,015,817 bytes per MUT second

      Productivity  60.2% of total user, 60.3% of total elapsed

    Preprocessing executable 'step-3' for profiling-0.0.0...
    Running step-3...
    1.557225645250835 ± 1.1412294953786132e-3
       2,001,336,368 bytes allocated in the heap
         500,277,528 bytes copied during GC
          78,787,680 bytes maximum residency (11 sample(s))
           1,312,904 bytes maximum slop
                 176 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      3802 colls,     0 par    0.263s   0.263s     0.0001s    0.0014s
      Gen  1        11 colls,     0 par    0.218s   0.217s     0.0198s    0.0799s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.827s  (  0.827s elapsed)
      GC      time    0.480s  (  0.480s elapsed)
      EXIT    time    0.003s  (  0.003s elapsed)
      Total   time    1.314s  (  1.311s elapsed)

      %GC     time      36.6%  (36.6% elapsed)

      Alloc rate    2,419,416,054 bytes per MUT second

      Productivity  63.4% of total user, 63.6% of total elapsed

    Preprocessing executable 'step-4' for profiling-0.0.0...
    Running step-4...
    1.5572256452508353 ± 1.1412294953786132e-3
       1,923,612,824 bytes allocated in the heap
         500,349,712 bytes copied during GC
          73,235,984 bytes maximum residency (11 sample(s))
           1,541,376 bytes maximum slop
                 175 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      3694 colls,     0 par    0.266s   0.266s     0.0001s    0.0013s
      Gen  1        11 colls,     0 par    0.218s   0.218s     0.0198s    0.0803s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.804s  (  0.804s elapsed)
      GC      time    0.484s  (  0.484s elapsed)
      EXIT    time    0.003s  (  0.003s elapsed)
      Total   time    1.294s  (  1.291s elapsed)

      %GC     time      37.4%  (37.5% elapsed)

      Alloc rate    2,392,267,685 bytes per MUT second

      Productivity  62.6% of total user, 62.7% of total elapsed

    Preprocessing executable 'step-5' for profiling-0.0.0...
    Running step-5...
    1.5572256452508353 ± 1.1412294953786132e-3
       1,227,341,560 bytes allocated in the heap
         260,261,240 bytes copied during GC
          46,564,552 bytes maximum residency (8 sample(s))
           2,502,120 bytes maximum slop
                 107 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      2377 colls,     0 par    0.126s   0.126s     0.0001s    0.0006s
      Gen  1         8 colls,     0 par    0.117s   0.117s     0.0146s    0.0455s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.412s  (  0.412s elapsed)
      GC      time    0.244s  (  0.243s elapsed)
      EXIT    time    0.006s  (  0.006s elapsed)
      Total   time    0.665s  (  0.662s elapsed)

      %GC     time      36.6%  (36.8% elapsed)

      Alloc rate    2,980,645,861 bytes per MUT second

      Productivity  63.3% of total user, 63.7% of total elapsed

    Preprocessing executable 'step-6' for profiling-0.0.0...
    Running step-6...
    1.5572256452508353 ± 1.1412294953786132e-3
       1,093,399,200 bytes allocated in the heap
           1,513,992 bytes copied during GC
              44,312 bytes maximum residency (2 sample(s))
              21,224 bytes maximum slop
                   1 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      2119 colls,     0 par    0.005s   0.005s     0.0000s    0.0000s
      Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0002s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.371s  (  0.371s elapsed)
      GC      time    0.006s  (  0.006s elapsed)
      EXIT    time    0.000s  (  0.000s elapsed)
      Total   time    0.380s  (  0.377s elapsed)

      %GC     time       1.5%  (1.5% elapsed)

      Alloc rate    2,943,449,404 bytes per MUT second

      Productivity  98.5% of total user, 99.3% of total elapsed

    Preprocessing executable 'step-7' for profiling-0.0.0...
    Running step-7...
    1.5572256452508353 ± 1.1412294953786132e-3
       1,074,199,184 bytes allocated in the heap
           1,183,136 bytes copied during GC
              44,312 bytes maximum residency (2 sample(s))
              21,224 bytes maximum slop
                   1 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      2086 colls,     0 par    0.005s   0.005s     0.0000s    0.0000s
      Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0002s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.372s  (  0.372s elapsed)
      GC      time    0.005s  (  0.005s elapsed)
      EXIT    time    0.000s  (  0.000s elapsed)
      Total   time    0.380s  (  0.377s elapsed)

      %GC     time       1.4%  (1.4% elapsed)

      Alloc rate    2,890,973,691 bytes per MUT second

      Productivity  98.6% of total user, 99.4% of total elapsed

    Preprocessing executable 'step-8' for profiling-0.0.0...
    Running step-8...
    1.562377823695294 ± 1.1220426138338487e-3
          49,755,944 bytes allocated in the heap
              20,272 bytes copied during GC
              44,312 bytes maximum residency (2 sample(s))
              25,320 bytes maximum slop
                   1 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0        95 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
      Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.014s  (  0.014s elapsed)
      GC      time    0.000s  (  0.000s elapsed)
      EXIT    time    0.000s  (  0.000s elapsed)
      Total   time    0.017s  (  0.014s elapsed)

      %GC     time       2.0%  (2.4% elapsed)

      Alloc rate    3,591,865,551 bytes per MUT second

      Productivity  97.4% of total user, 117.6% of total elapsed

    Preprocessing executable 'step-9' for profiling-0.0.0...
    Running step-9...
    1.562377823695294 ± 1.1220426138338487e-3
          38,553,192 bytes allocated in the heap
              20,160 bytes copied during GC
              44,312 bytes maximum residency (2 sample(s))
              25,320 bytes maximum slop
                   1 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0        73 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
      Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

      INIT    time    0.000s  (  0.000s elapsed)
      MUT     time    0.013s  (  0.013s elapsed)
      GC      time    0.000s  (  0.000s elapsed)
      EXIT    time    0.000s  (  0.000s elapsed)
      Total   time    0.016s  (  0.013s elapsed)

      %GC     time       1.9%  (2.3% elapsed)

      Alloc rate    3,010,451,374 bytes per MUT second

      Productivity  97.4% of total user, 118.8% of total elapsed

Run 10000000 iterations in 3 dimensions for the last two steps.

    $ for n in $(seq 9 10) ; do time dist/build/step-$n/step-$n 10000000 3 ; done
    1.5626411480119926 ± 1.1296171263256967e-4

    real    0m1.271s
    user    0m1.267s
    sys     0m0.004s
    1.5626411480119926 ± 1.1296171263256967e-4

    real    0m1.087s
    user    0m1.083s
    sys     0m0.004s
