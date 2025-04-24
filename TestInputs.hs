module TestInputs where
import Checkers

initialGame :: Game
initialGame = (White,whitePieces++blackPieces,100)
    where whitePieces = [((1,1),(False,White)),((3,1),(False,White)),((5,1),(False,White)),((7,1),(False,White)),
                         ((2,2),(False,White)),((4,2),(False,White)),((6,2),(False,White)),((8,2),(False,White)),
                         ((1,3),(False,White)),((3,3),(False,White)),((5,3),(False,White)),((7,3),(False,White))]
          blackPieces = [((8,8),(False,Black)),((6,8),(False,Black)),((4,8),(False,Black)),((2,8),(False,Black)),
                         ((7,7),(False,Black)),((5,7),(False,Black)),((3,7),(False,Black)),((1,7),(False,Black)),
                         ((8,6),(False,Black)),((6,6),(False,Black)),((4,6),(False,Black)),((2,6),(False,Black))]

game1 :: Game
game1 = (Black,whitePieces++blackPieces,99)
    where whitePieces = [((1,1),(False,White)),((3,1),(False,White)),((5,1),(False,White)),((7,1),(False,White)),
                         ((2,2),(False,White)),((4,2),(False,White)),((6,2),(False,White)),((8,2),(False,White)),
                         ((1,3),(False,White)),((3,3),(False,White)),((5,3),(False,White)),
                         ((8,4),(False,White))]
          blackPieces = [((8,8),(False,Black)),((6,8),(False,Black)),((4,8),(False,Black)),((2,8),(False,Black)),
                         ((7,7),(False,Black)),((5,7),(False,Black)),((3,7),(False,Black)),((1,7),(False,Black)),
                         ((8,6),(False,Black)),((6,6),(False,Black)),((4,6),(False,Black)),((2,6),(False,Black))]

jumpGame :: Game
jumpGame = (Black,whitePieces++blackPieces,50)
    where whitePieces = [((1,1),(False,White)),((3,1),(False,White)),((5,1),(False,White)),((7,1),(False,White)),
                         ((2,2),(False,White)),((4,2),(False,White)),((6,2),(False,White)),((8,2),(False,White)),
                         ((1,3),(False,White)),((3,3),(False,White)),
                         ((6,4),(False,White))]
          blackPieces = [((8,8),(False,Black)),((6,8),(False,Black)),((4,8),(False,Black)),((2,8),(False,Black)),
                         ((7,7),(False,Black)),((5,7),(False,Black)),((3,7),(False,Black)),((1,7),(False,Black)),
                         ((8,6),(False,Black)),((6,6),(False,Black)),((2,6),(False,Black)),
                         ((7,5),(False,Black))]

kingGame :: Game
kingGame = (White,[((7,7),(False,White)),((2,2),(False,Black))],50)

kingGameB :: Game
kingGameB = (Black,[((7,7),(False,White)),((2,2),(False,Black))],50)
