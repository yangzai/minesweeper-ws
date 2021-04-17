# Minesweeper over Websockets
- `ServerGenerator`: Minesweeper puzzle generator
- `ClientSolver`: Minesweeper puzzle solver

## Requirements
- JDK >= 11

## Solver Limitations / Future Work
- Early guess clustering:
    * Current implementation did not include closed cells bordering marked bombs as border cells.
      There might be cases where a cluster is solely enclosed by bombs and this will not be detected for early guessing.
    * Current implementation only detects clusters will all border cells for guessing.
      There are cases where clusters with non-border cells would also benefit from early guessing but that would require
      clustering of all closed cells to determine (costly as size of grid increases),
      instead of clustering of only border cells we are currently doing.
- Guessing candidate selection:
    * Current selection algorithm based on 1st degree neighbours is too simplistic.
      Probability ranking should be based on reduced constraints.
    * We might want to add some randomness if there are multiple candidates with the same rank.
- Tautological evidence from the consideration of all possible valid states:
    * There might be situations where tautological evidence about a certain cell can only be derived
      from the consideration of all valid permutation of valid states of a cluster, or the whole board (e.g. a cell is
      always a bomb or always safe in all valid permutations). This might be more useful if we have knowledge of
      the total number of bombs since the only way to apply such a total bomb count constraint
      is to consider if each possible permutation fulfills the bomb count requirement.
- Performance:
    * There might be memoisation opportunities to reduce recomputation (in e.g. clustering, constraint reduction) but
      this would introduce more complexity in memoised data.
    * Separate fibers for gameplay(send) and response handling(receive). Gameplay needs to wait on map updates and
      needs to be interrupted on game end.
