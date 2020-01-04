import qualified Data.Array as A
import qualified Control.Monad.Writer.Lazy as W
import Control.Applicative
import Data.Maybe
import Debug.Trace

-- This type represents a number which represents an input to, an instruction from, or an output from an Intcode program.
-- This is a separate type in case we need to switch ProgUnit to arbitrary-precision Integer later
type ProgUnit = Int

fromProgUnit :: ProgUnit -> Int
fromProgUnit = id

toProgUnit :: Int -> ProgUnit
toProgUnit = id

rawProgram :: [ProgUnit]
rawProgram = [3,62,1001,62,11,10,109,2241,105,1,0,2066,1569,571,1239,2200,1342,2169,899,1173,2035,1540,1204,1637,1806,641,977,1272,1773,1144,1008,1699,2107,1107,1909,1872,833,802,1410,932,1072,1307,1938,2000,734,1837,2138,1039,1606,701,1373,608,866,1443,1478,771,1509,672,1969,1668,1738,0,0,0,0,0,0,0,0,0,0,0,0,3,64,1008,64,-1,62,1006,62,88,1006,61,170,1105,1,73,3,65,20102,1,64,1,21002,66,1,2,21102,105,1,0,1106,0,436,1201,1,-1,64,1007,64,0,62,1005,62,73,7,64,67,62,1006,62,73,1002,64,2,132,1,132,68,132,1002,0,1,62,1001,132,1,140,8,0,65,63,2,63,62,62,1005,62,73,1002,64,2,161,1,161,68,161,1102,1,1,0,1001,161,1,169,101,0,65,0,1102,1,1,61,1102,1,0,63,7,63,67,62,1006,62,203,1002,63,2,194,1,68,194,194,1006,0,73,1001,63,1,63,1106,0,178,21102,210,1,0,105,1,69,1202,1,1,70,1101,0,0,63,7,63,71,62,1006,62,250,1002,63,2,234,1,72,234,234,4,0,101,1,234,240,4,0,4,70,1001,63,1,63,1106,0,218,1105,1,73,109,4,21102,1,0,-3,21102,0,1,-2,20207,-2,67,-1,1206,-1,293,1202,-2,2,283,101,1,283,283,1,68,283,283,22001,0,-3,-3,21201,-2,1,-2,1106,0,263,21202,-3,1,-3,109,-4,2105,1,0,109,4,21101,1,0,-3,21101,0,0,-2,20207,-2,67,-1,1206,-1,342,1202,-2,2,332,101,1,332,332,1,68,332,332,22002,0,-3,-3,21201,-2,1,-2,1105,1,312,21202,-3,1,-3,109,-4,2105,1,0,109,1,101,1,68,359,20101,0,0,1,101,3,68,367,20102,1,0,2,21101,0,376,0,1105,1,436,22101,0,1,0,109,-1,2106,0,0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,35184372088832,70368744177664,140737488355328,281474976710656,562949953421312,1125899906842624,109,8,21202,-6,10,-5,22207,-7,-5,-5,1205,-5,521,21101,0,0,-4,21101,0,0,-3,21101,0,51,-2,21201,-2,-1,-2,1201,-2,385,471,20102,1,0,-1,21202,-3,2,-3,22207,-7,-1,-5,1205,-5,496,21201,-3,1,-3,22102,-1,-1,-5,22201,-7,-5,-7,22207,-3,-6,-5,1205,-5,515,22102,-1,-6,-5,22201,-3,-5,-3,22201,-1,-4,-4,1205,-2,461,1106,0,547,21102,1,-1,-4,21202,-6,-1,-6,21207,-7,0,-5,1205,-5,547,22201,-7,-6,-7,21201,-4,1,-4,1105,1,529,21201,-4,0,-7,109,-8,2106,0,0,109,1,101,1,68,563,21002,0,1,0,109,-1,2106,0,0,1102,6581,1,66,1101,0,4,67,1101,598,0,68,1101,0,253,69,1101,0,1,71,1101,606,0,72,1106,0,73,0,0,0,0,0,0,0,0,27,85159,1102,6151,1,66,1101,0,2,67,1102,635,1,68,1101,302,0,69,1102,1,1,71,1101,0,639,72,1105,1,73,0,0,0,0,1,292724,1102,84653,1,66,1101,0,1,67,1101,0,668,68,1101,0,556,69,1101,0,1,71,1102,1,670,72,1106,0,73,1,11,34,14669,1102,96893,1,66,1101,0,1,67,1102,699,1,68,1102,556,1,69,1102,0,1,71,1102,701,1,72,1106,0,73,1,1705,1101,93557,0,66,1101,2,0,67,1101,728,0,68,1102,302,1,69,1101,1,0,71,1102,1,732,72,1106,0,73,0,0,0,0,41,155138,1101,102061,0,66,1101,4,0,67,1102,761,1,68,1102,1,302,69,1102,1,1,71,1102,1,769,72,1105,1,73,0,0,0,0,0,0,0,0,4,122394,1101,0,93497,66,1102,1,1,67,1102,1,798,68,1101,556,0,69,1101,1,0,71,1102,800,1,72,1106,0,73,1,-18,49,41897,1101,0,80071,66,1102,1,1,67,1101,0,829,68,1101,0,556,69,1102,1,1,71,1102,1,831,72,1105,1,73,1,-1474987,2,13162,1101,71473,0,66,1101,2,0,67,1102,860,1,68,1101,0,351,69,1102,1,1,71,1102,1,864,72,1105,1,73,0,0,0,0,255,32771,1101,77569,0,66,1101,0,2,67,1102,1,893,68,1102,1,302,69,1101,1,0,71,1102,1,897,72,1106,0,73,0,0,0,0,39,52396,1102,24799,1,66,1102,1,1,67,1101,926,0,68,1101,0,556,69,1101,2,0,71,1102,1,928,72,1105,1,73,1,211,39,13099,11,42299,1102,55399,1,66,1102,1,1,67,1101,959,0,68,1102,1,556,69,1101,8,0,71,1102,961,1,72,1105,1,73,1,2,41,77569,39,26198,27,170318,17,79714,3,78889,34,44007,4,61197,4,81596,1101,0,22993,66,1101,1,0,67,1102,1,1004,68,1102,1,556,69,1102,1,1,71,1101,0,1006,72,1105,1,73,1,2485433,2,19743,1101,0,32687,66,1101,0,1,67,1101,0,1035,68,1102,556,1,69,1102,1,1,71,1102,1037,1,72,1106,0,73,1,28661,16,15501,1102,1,44657,66,1101,1,0,67,1102,1,1066,68,1102,556,1,69,1102,1,2,71,1102,1,1068,72,1106,0,73,1,10,33,102061,4,40798,1102,53777,1,66,1101,3,0,67,1101,1099,0,68,1101,0,302,69,1101,0,1,71,1101,0,1105,72,1106,0,73,0,0,0,0,0,0,22,212991,1101,70997,0,66,1101,4,0,67,1102,1,1134,68,1102,1,253,69,1101,0,1,71,1101,0,1142,72,1106,0,73,0,0,0,0,0,0,0,0,38,93557,1102,93761,1,66,1101,1,0,67,1102,1171,1,68,1102,556,1,69,1102,1,0,71,1102,1,1173,72,1106,0,73,1,1625,1101,0,96973,66,1102,1,1,67,1102,1,1200,68,1102,556,1,69,1102,1,1,71,1102,1202,1,72,1105,1,73,1,13,24,102071,1101,0,42299,66,1102,1,3,67,1101,0,1231,68,1101,302,0,69,1102,1,1,71,1101,0,1237,72,1105,1,73,0,0,0,0,0,0,42,82763,1102,78889,1,66,1102,2,1,67,1102,1,1266,68,1102,1,302,69,1102,1,1,71,1102,1270,1,72,1105,1,73,0,0,0,0,34,29338,1102,5167,1,66,1102,3,1,67,1101,0,1299,68,1101,0,302,69,1101,0,1,71,1102,1,1305,72,1106,0,73,0,0,0,0,0,0,22,70997,1101,12697,0,66,1101,0,1,67,1101,1334,0,68,1101,0,556,69,1101,3,0,71,1101,0,1336,72,1105,1,73,1,5,33,306183,33,408244,4,20399,1102,1,99251,66,1101,0,1,67,1102,1369,1,68,1102,556,1,69,1102,1,1,71,1101,0,1371,72,1106,0,73,1,181,24,204142,1102,1,13099,66,1102,1,4,67,1102,1400,1,68,1101,302,0,69,1102,1,1,71,1102,1408,1,72,1106,0,73,0,0,0,0,0,0,0,0,1,73181,1102,1,85159,66,1102,1,2,67,1102,1437,1,68,1102,1,302,69,1101,1,0,71,1101,0,1441,72,1106,0,73,0,0,0,0,17,39857,1102,1,82763,66,1102,1,3,67,1101,1470,0,68,1102,1,302,69,1102,1,1,71,1101,0,1476,72,1106,0,73,0,0,0,0,0,0,1,146362,1102,44179,1,66,1102,1,1,67,1101,1505,0,68,1102,556,1,69,1102,1,1,71,1102,1507,1,72,1105,1,73,1,278,29,161331,1102,1,19457,66,1102,1,1,67,1101,1536,0,68,1102,1,556,69,1102,1,1,71,1102,1,1538,72,1105,1,73,1,17,39,39297,1101,0,60251,66,1101,0,1,67,1102,1567,1,68,1102,1,556,69,1102,1,0,71,1101,1569,0,72,1106,0,73,1,1621,1101,73181,0,66,1102,4,1,67,1101,0,1596,68,1101,0,253,69,1102,1,1,71,1102,1,1604,72,1105,1,73,0,0,0,0,0,0,0,0,25,71473,1101,0,99277,66,1102,1,1,67,1102,1633,1,68,1102,556,1,69,1101,1,0,71,1102,1635,1,72,1106,0,73,1,-661,24,408284,1102,1,2957,66,1101,1,0,67,1101,1664,0,68,1101,556,0,69,1101,1,0,71,1101,1666,0,72,1106,0,73,1,16,38,187114,1102,77687,1,66,1101,0,1,67,1101,1695,0,68,1102,1,556,69,1101,1,0,71,1101,1697,0,72,1105,1,73,1,160,4,101995,1102,79757,1,66,1102,1,1,67,1102,1,1726,68,1101,0,556,69,1101,5,0,71,1102,1728,1,72,1105,1,73,1,1,49,125691,29,53777,24,306213,16,5167,11,84598,1102,41897,1,66,1102,3,1,67,1102,1,1765,68,1102,302,1,69,1102,1,1,71,1102,1,1771,72,1105,1,73,0,0,0,0,0,0,22,141994,1102,1,39857,66,1102,2,1,67,1102,1800,1,68,1102,302,1,69,1102,1,1,71,1101,0,1804,72,1105,1,73,0,0,0,0,3,157778,1101,87671,0,66,1101,1,0,67,1102,1833,1,68,1102,556,1,69,1101,1,0,71,1101,1835,0,72,1105,1,73,1,64937,49,83794,1102,14669,1,66,1102,1,3,67,1102,1864,1,68,1101,0,302,69,1101,1,0,71,1101,0,1870,72,1105,1,73,0,0,0,0,0,0,40,6151,1101,0,102071,66,1102,4,1,67,1101,1899,0,68,1101,302,0,69,1102,1,1,71,1101,1907,0,72,1106,0,73,0,0,0,0,0,0,0,0,22,283988,1101,93169,0,66,1101,1,0,67,1102,1936,1,68,1101,0,556,69,1101,0,0,71,1101,1938,0,72,1105,1,73,1,1087,1102,1,92173,66,1101,0,1,67,1102,1,1965,68,1102,1,556,69,1101,1,0,71,1101,0,1967,72,1105,1,73,1,7751410,2,26324,1101,80897,0,66,1101,1,0,67,1102,1996,1,68,1101,0,556,69,1101,1,0,71,1102,1,1998,72,1106,0,73,1,-1275519,2,6581,1101,0,102481,66,1101,3,0,67,1101,2027,0,68,1102,302,1,69,1101,0,1,71,1101,0,2033,72,1106,0,73,0,0,0,0,0,0,1,219543,1102,1,2687,66,1102,1,1,67,1101,2062,0,68,1102,1,556,69,1101,1,0,71,1102,1,2064,72,1106,0,73,1,5807,29,107554,1101,0,32771,66,1101,0,1,67,1102,2093,1,68,1101,0,556,69,1102,6,1,71,1101,0,2095,72,1105,1,73,1,22083,40,12302,42,165526,42,248289,32,102481,32,204962,32,307443,1102,2791,1,66,1101,0,1,67,1101,0,2134,68,1101,0,556,69,1101,1,0,71,1102,2136,1,72,1106,0,73,1,-204,11,126897,1101,5669,0,66,1102,1,1,67,1102,2165,1,68,1101,556,0,69,1101,1,0,71,1101,2167,0,72,1106,0,73,1,125,33,204122,1101,94693,0,66,1102,1,1,67,1101,2196,0,68,1101,556,0,69,1101,1,0,71,1101,2198,0,72,1105,1,73,1,244,16,10334,1101,0,20399,66,1101,0,6,67,1102,1,2227,68,1101,0,302,69,1101,1,0,71,1102,1,2239,72,1106,0,73,0,0,0,0,0,0,0,0,0,0,0,0,25,142946]

-- A Program is represented as an Array of ProgUnits
type Program = A.Array Int ProgUnit

-- Convert rawProgram, a simple list of ProgUnits, into an actual Array
program :: Program
program = A.listArray (0, 3000) (rawProgram ++ (repeat 0))

-- Mode represents the types of addressing modes in Intcode programs
-- Notice that toEnum 0 is Position, toEnum 1 is Immediate, and toEnum 2 is Relative
-- This fact will be used later in the Intcode interpreter
data Mode = Position | Immediate | Relative deriving (Enum)

-- A ProgState is essentially a triplet of (Int, Int, Program)
-- The first Int represents the current relative base
-- The second Int represents the index of the current instruction
-- The third Int represents the current state of the program data/instructions
data ProgState = ProgState Int Int Program deriving (Show)

-- The output of a program is a simple list of ProgUnits
type ProgOutput = [ProgUnit]
-- This monad is used to represent both the output of a program and the current state of a program
type ProgWriter = W.Writer ProgOutput ProgState

-- This is a helper function used below:
compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

-- contProgram takes in a list of ProgUnits representing inputs to the program, and a ProgState, and returns a ProgWriter
-- The ProgWriter contains a ProgOutput representing the outputs of the program and a ProgState representing the new state of the program
-- The new state of the program returned from this function always has a current op code of 99, in which case the program has terminated, or an opcode of 3, in which case the program has used all of the input given and is waiting for more input.
contProgram :: [ProgUnit] -> ProgState -> ProgWriter
contProgram inputs state@(ProgState relBase curLoc prog) =
  -- First, let's read the current op code and the modes used for reading all three parameters:
  let infoCell = fromProgUnit $ prog A.! curLoc
      opCode = infoCell `mod` 100
      mode1 = toEnum $ (infoCell `quot` 100) `mod` 10 
      mode2 = toEnum $ (infoCell `quot` 1000) `mod` 10
      mode3 = toEnum $ (infoCell `quot` 10000) `mod` 10 in
  -- If the current op code is 99, then we're done, so just return the current ProgState:
  case opCode of
    99 -> return state
    -- Otherwise, make sure there is enough space in the array to read in the parameters for the current opCode:
    _ -> let (_, lastInd) = A.bounds prog
             lastParamLoc = curLoc+(numParams opCode) in
         if lastParamLoc <= lastInd then
           -- runP is a helper function which takes in an Int representing the index of an instruction and a Program representing the new state of the program data, and continues running the program with the same inputs and relative base as before:
           let runP = (contProgram inputs) `compose2` (ProgState relBase) in
           -- If the op code is 1 or 2, just do the addition or multiplication operator and then move the index of the current instruction 4 spaces over:
           case opCode of
             1 -> runP (curLoc+4) $ calcOp mode1 mode2 mode3 (+)
             2 -> runP (curLoc+4) $ calcOp mode1 mode2 mode3 (*)

             -- If the op code is 3, then take one of the inputs and put it in the appropriate place in the program data. Then, move the instruction index 2 spaces over.
             -- If there are no more inputs, then we can not continue execution, so just return the current ProgState.
             3 -> case inputs of
                    [] -> return state
                    input:rst ->
                      contProgram rst $ ProgState relBase (curLoc+2) $
                         prog A.// [((readLoc mode1 (curLoc+1)), input)]

             -- If the op code is 4, then add the appropriate element to the ProgOutput, move the instruction index 2 spaces over, and continue execution.
             4 -> (W.tell [readElem mode1 (curLoc+1)]) >>
                    runP (curLoc+2) prog

             -- If the op code is 5, then jump the instruction index to the appropriate location **if the first parameter is non-zero**. Otherwise, just move the instruction index 3 steps over.
             5 -> if ((readElem mode1 (curLoc+1)) == 0)
                    then runP (curLoc+3) prog
                    else runP (fromProgUnit $ readElem mode2 (curLoc+2)) prog
             -- If the op code is 6, then jump the instruction index to the appropriate location **if the first parameter is zero**. Otherwise, just move the instruction index 3 steps over.
             6 -> if ((readElem mode1 (curLoc+1)) /= 0)
                    then runP (curLoc+3) prog
                    else runP (fromProgUnit $ readElem mode2 (curLoc+2)) prog

             -- If the op code is 7 or 8, then do the comparison/equality operation and then move the index of the current instruction 4 spaces over:
             -- Notice that the (toProgUnit . fromEnum) function means that the (<) or (==) operator returns 0 if the Boolean value is False and 1 if the Boolean value is True.
             7 -> runP (curLoc+4) $
                    calcOp mode1 mode2 mode3
                           ((toProgUnit . fromEnum) `compose2` (<))
             8 -> runP (curLoc+4) $
                    calcOp mode1 mode2 mode3
                           ((toProgUnit . fromEnum) `compose2` (==))

             -- If the op code is 9, then add the relative base by the first parameter, move the index of the current instruction 2 spaces over, and continue execution:
             9 -> contProgram inputs $
                    ProgState (relBase+(fromProgUnit $ readElem mode1 (curLoc+1)))
                              (curLoc+2)
                              prog

             -- Otherwise, if this is an unrecognized op code, then output an error saying so:
             _ -> error "Unrecognized opcode"
         -- If there is not enough space in the array to read in the parameters of the given op code, then output an error saying so:
         else error "Not enough parameters"
  where -- This function takes in an op code and says how many parameters that op code needs:
        numParams 1 = 3
        numParams 2 = 3
        numParams 3 = 1
        numParams 4 = 1
        numParams 5 = 2
        numParams 6 = 2
        numParams 7 = 3
        numParams 8 = 3
        numParams 9 = 1
        numParams _ = 0

        -- This function takes in a mode and a location representing a parameter in the program array and then outputs the location of the value corresponding to that parameter in the program array
        readLoc Position loc = fromProgUnit $ prog A.! loc
        readLoc Immediate loc = loc
        readLoc Relative loc = relBase+(fromProgUnit $ prog A.! loc)

        -- This function takes in a mode and a location representing a parameter in the program array and then outputs the value corresponding to that parameter
        readElem m loc = prog A.! (readLoc m loc)

        -- This function takes in the modes of each of the three parameters of the current op code, along with a binary function on the Integers, and computes the result of the binary function on the first two parameters, storing the result in the location of the third parameter.
        calcOp mode1 mode2 mode3 f =
          let firstElem = readElem mode1 (curLoc+1)
              secondElem = readElem mode2 (curLoc+2)
              destLoc = readLoc mode3 (curLoc+3) in
          prog A.// [(destLoc, f firstElem secondElem)]

-- startProgram takes in a Program and a list of ProgUnits representing the initial input to the program, and outputs a ProgWriter representing the program state after running the given Program on the given input
startProgram :: Program -> [ProgUnit] -> ProgWriter
startProgram prog inputs = contProgram inputs $ ProgState 0 0 prog

-- For any i in [0..49], (initialStates !! i) represents the initial program state of the computer on the network with address i
initialStates :: [ProgWriter]
initialStates = map (startProgram program . (:[])) [0..49]

-- A Packet is essentially a triplet of ProgUnits
-- The first ProgUnit represents an address
-- The second ProgUnit represents an x-coordinate
-- The third ProgUnit represents a y-coordinate
data Packet = Packet { getAddress :: ProgUnit, getX :: ProgUnit, getY :: ProgUnit } deriving (Show)

-- A NAT is a Maybe Packet, which represents the last packet sent to address 255
-- It is a Maybe object because, at the beginning of the program before any packets have been set, the NAT has not received any packets yet, so its value is Nothing
type NAT = Maybe Packet

-- Programs output a list of ProgUnits representing Packets, so this function converts that list of ProgUnits to actual Packet structures:
parseOutput :: [ProgUnit] -> [Packet]
parseOutput (a:b:c:rst) = Packet a b c : parseOutput rst
parseOutput []          = []
parseOutput _           = error "Invalid number of elements in output"

-- A NetworkState is essentially a triplet of ([ProgWriter], NAT, Int)
-- The [ProgWriter] represents the program states and outputs of all 50 computers
-- (Because of how contProgram works, in a NetworkState, all of the ProgWriters are waiting on an input instruction.)
-- The NAT represents the last packet the NAT received
-- The Int represents the number of cycles since a packet has been set
-- To be clear, a "cycle" is one call of the updateNetwork function
data NetworkState = NetworkState { getProgStates :: [ProgWriter], getNat :: NAT, getIdleCount :: Int } deriving (Show)

-- initialNetworkState represents the initial state of the network, before any packets have been set
initialNetworkState :: NetworkState
initialNetworkState = NetworkState initialStates Nothing 0

-- idleThreshold is the number of cycles before the network is considered "idle"
idleThreshold :: Int
idleThreshold = 100

-- updateNetwork takes all of the packets sent in the outputs of the ProgWriter of the given NetworkState, sends them as input to the appropriate 
updateNetwork :: NetworkState -> NetworkState
updateNetwork (NetworkState curProgStates curNat idleCount) =
  -- Create a new NetworkState using the newInputs, newNat, and newIdleCount calculated below:
  NetworkState (zipWith updateState curProgStates newInputs)
               newNat
               newIdleCount
  where -- This variable represents all of the packets sent by all of the ProgWriters in the current NetworkState
        allPackets = curProgStates >>= (parseOutput . W.execWriter)

        -- Does the given Packet match the given address of the computer?
        packetMatches compAddress (Packet packAddress _ _) =
          compAddress == packAddress

        -- Convert a Packet to a list of ProgUnits to send as input to an Intcode program:
        packetToInput (Packet _ x y) = [x, y]

        -- If the list of inputs to a program is empty, replace that input with a single -1 to represent that this program will receive no packets:
        updateDefaultInput [] = [-1]
        updateDefaultInput lst = lst

        -- Now, calculate the newInputs, newNat, and newIdleCount:
        (newInputs, newNat, newIdleCount) =
          case allPackets of
            -- If no programs have sent any packets, then the network is idle:
            []
              -- If the network has been idle for idleThreshold cycles, then 
              | idleCount >= idleThreshold ->
                (-- If the NAT object is not Nothing, then send the appropriate x and y coordinates to address 0 and send a -1 to all of the other computers:
                 case curNat of
                   Nothing     -> repeat [-1]
                   Just packet -> traceShow packet $
                                  packetToInput packet : repeat [-1],
                 curNat,
                 -- Also, since a packet from the NAT has been sent, reset the idle count to 0:
                 0)
              -- Otherwise, if the network has been idle for idleThreshold cycles yet, then just send -1 to all of the programs and increment the idleCount by 1:
              | otherwise -> (repeat [-1], curNat, idleCount+1)

            -- In this case, at least one program has sent a packet and the network is not idle:
            _  -> (-- To create newInputs, do the following operations:
                   -- First, for the computer at address i, find all of the packets in allPackets which match the computer address.
                   -- Then, convert all of the packets to list of ProgUnits and concatenate those lists together.
                   -- Finally, if the result of the above is simply an empty list [], then no packets have been sent to this program, so replace the empty list with a single [-1]
                   map (updateDefaultInput .
                         (>>= packetToInput) .
                           flip filter allPackets . packetMatches)
                       [0..49],
                   -- To update the NAT object, first, find all of the packets which match address 255.
                   let packetsTo255 =
                         flip filter allPackets $ packetMatches 255
                       -- listToMaybe $ reverse packetsTo255 will return the last packet sent to address 255 if there is one, and Nothing otherwise.
                       possibleNewNat = listToMaybe $ reverse packetsTo255 in
                     -- Here, the <|> function will return possibleNewNat if it is not Nothing (i.e. if there was a new packet sent to address 255), and will return curNat if possibleNewNat is Nothing (i.e. keep the current NAT object if no packet was sent to address 255 in this cycle
                     possibleNewNat <|> curNat,
                   -- Since the network is not idle, reset the idle count:
                   0)

        -- Given the current program state and a list of ProgUnits representing packets sent to this computer,
        -- updateState clears the output of the current computer and then pipes the current program state into contProgram in order to update the program state with the new input and get any new output of the program.
        updateState curProgState newInput =
          (W.censor (const []) curProgState) >>= contProgram newInput

-- detectPacketsSentByNat takes in a list of consecutive NetworkStates, looks for changes in the idle counts to see when the idle threshold was met and the idle counter was reset, and then returns a list of all of Packets sent by the NAT
detectPacketsSentByNat :: [NetworkState] -> [Packet]
detectPacketsSentByNat ((NetworkState _ natSent idle1):
                        rstLst@((NetworkState _ _ idle2):_))
  | (idle1 == idleThreshold) && (idle2 == 0) =
    case natSent of
      Nothing     -> rstPackets
      Just packet -> packet : rstPackets
  | otherwise     = rstPackets
  where rstPackets = detectPacketsSentByNat rstLst
detectPacketsSentByNat _ = []

-- findFirstRepeat takes a list of objects and finds the first object which repeats itself twice in a row
findFirstRepeat :: (Eq a) => [a] -> a
findFirstRepeat (a:rstLst@(b:_))
  | a == b    = a
  | otherwise = findFirstRepeat rstLst
findFirstRepeat _ = error "No repeated element found"

-- Find the first repeat in the y-coordinates of the packets sent by the NAT object:
main :: IO ()
main = print $ findFirstRepeat $ map getY $ detectPacketsSentByNat $ iterate updateNetwork initialNetworkState