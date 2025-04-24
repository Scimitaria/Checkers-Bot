module TestInputs where
import Checkers

initialGame :: Game
initialGame = (White, whitePieces++blackPieces,100)
    where whitePieces = [((1,1),(False,White)),
                         ((3,1),(False,White)),
                         ((5,1),(False,White)),
                         ((7,1),(False,White)),
                         ((2,2),(False,White)),
                         ((4,2),(False,White)),
                         ((6,2),(False,White)),
                         ((8,2),(False,White)),
                         ((1,3),(False,White)),
                         ((3,3),(False,White)),
                         ((5,3),(False,White)),
                         ((7,3),(False,White))]
          blackPieces = [((8,8),(False,Black)),
                         ((6,8),(False,Black)),
                         ((4,8),(False,Black)),
                         ((2,8),(False,Black)),
                         ((7,7),(False,Black)),
                         ((5,7),(False,Black)),
                         ((3,7),(False,Black)),
                         ((1,7),(False,Black)),
                         ((8,6),(False,Black)),
                         ((6,6),(False,Black)),
                         ((4,6),(False,Black)),
                         ((2,6),(False,Black))]
