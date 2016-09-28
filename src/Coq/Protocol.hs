-- | Data types for the protocol between @coqtop@ and an editor.
module Coq.Protocol
  (StateId (..),
   -- * @Init@
   Init (..), InitResp (..),
   -- * @About@
   About (..), AboutResp (..),
   -- * @Status@
   Status (..), StatusResp (..),
   -- * @Add@
   Add (..), AddResp (..),
   -- * @EditAt@
   EditAt (..), EditAtResp (..),
   -- * @Query@
   Query (..), QueryResp (..),
   -- * @Goals@
   Goal (..), GoalResp (..), GoalInfo (..),
   -- * @Evars@
   Evars (..), EvarsResp (..), Evar (..),
   -- * @Hints@
   Hints (..), HintsResp (..), Hint (..),
   -- * @Search@
   Search (..), SearchResp (..),
   SearchFlag (..), SearchConstraint (..), CoqObject (..),
   -- * @GetOptions@
   GetOptions (..), GetOptionsResp (..),
   Option (..), OptionState (..), OptionValue (..),
   -- * @SetOptions@
   SetOptions (..), SetOptionsResp (..), SetOption (..),
   -- * @MakeCases@
   MakeCases (..), MakeCasesResp (..),
   -- * @Quit@
   Quit (..), QuitResp (..))
where

import Data.Text (Text)


-- | An identifier for a particular @coqtop@ state.
newtype StateId = StateId Int
  deriving (Eq, Show)


-- | Load a file or start an empty session.
--
-- If loading an existing file, then 'Init' also adds its directory to the
-- load path if necessary, and loads compilation hints for its filename.
data Init =
    Init {
      -- | 'Nothing' to begin an empty session.
      iFilename :: Maybe Text
    }
  deriving (Eq, Show)

-- | 'Init' response.
data InitResp =
    InitResp {
      -- State ID for the start of the file
      irStateId :: StateId
    }
  deriving (Eq, Show)


-- | Ask for information about the @coqtop@ executable itself.
data About = About deriving (Eq, Show)

-- | 'About' response.
data AboutResp =
    AboutResp {
      arCoqtopVersion, arProtocolVersion,
        arReleaseDate, arCompileDate :: Text
    }
  deriving (Eq, Show)


-- | Current prover status.
newtype Status =
    Status {
      -- | Whether to first force evaluation of any unevaluated statements
      sForceEval :: Bool
    }
  deriving (Eq, Show)

-- | 'Status' response.
data StatusResp =
    StatusResp {
      -- | Module path of the current proof, if any
      srProofPath :: [Text],
      -- | Current proof name, if any
      srProofName :: Maybe Text,
      -- | All pending obligations, in an unspecified order
      srAllProofs :: [Text],
      -- | An identifier for the current proof's state
      -- TODO: what does that mean?
      srProofNum  :: Int
    }
  deriving (Eq, Show)


-- | Send some input to @coqtop@.
data Add =
    Add {
      aPhrase  :: Text,
      -- | TODO: ???
      aEditId  :: Int,
      -- | Expected current state
      aStateId :: StateId,
      aVerbose :: Bool
    }
  deriving (Eq, Show)

-- | 'Add' response.
data AddResp =
    AddResp {
      -- | State ID for the phrase passed in
      arStateId   :: StateId,
      -- | New edit point, if different
      arEditPoint :: Maybe StateId,
      arMessage   :: Text
    }
  deriving (Eq, Show)


-- | Set the current edit point to a previous location.
newtype EditAt =
    EditAt {
      -- | State ID of the phrase to rewind to
      eStateId :: StateId
    }
  deriving (Eq, Show)

-- | 'EditAt' response.
data EditAtResp =
    -- | Rewound to requested point
    EditAtNewTip
    -- | Rewound to @tip@, which is in a focusable zone delimited by
    -- @start@ and @stop@
  | EditAtFocus { erStart, erStop, erTip :: StateId }
  deriving (Eq, Show)


-- | Execute a query (e.g. @Print@, @Check@, ...).
data Query =
    Query {
      qQuery :: Text,
      -- | Expected current state
      qStateId :: StateId
    }
  deriving (Eq, Show)

-- | 'Query' response.
newtype QueryResp = QueryResp { qrMessage :: Text }
  deriving (Eq, Show)


-- | Ask for the current goals.
data Goal = Goal
  deriving (Eq, Show)

-- | 'Goal' response.
data GoalResp =
    -- | No current proof
    GoalNotInProof
  | GoalResp {
      -- | Currently focused goal(s)
      grForeground :: [GoalInfo],
      -- | Unfocused goals (zipper)
      grBackground :: ([GoalInfo], [GoalInfo]),
      -- | Previously deferred goals
      grShelved :: [GoalInfo],
      -- | Previous goals \"proven\" by @admit@
      grGivenUp :: [GoalInfo]
    }
  deriving (Eq, Show)

-- | A single goal.
data GoalInfo =
    GoalInfo {
      -- | A unique identifier
      gGoalId :: Text,
      gHypotheses :: [Text],
      gConclusion :: Text
    }
  deriving (Eq, Show)


-- | Ask for the currently uninstantiated evars (existential variables) in
-- a proof.
data Evars = Evars
  deriving (Eq, Show)

-- | 'Evars' response.
data EvarsResp =
    EvarsNotInProof
  | EvarsResp { eEvars :: [Evar] }
  deriving (Eq, Show)

-- | Description of an evar.
newtype Evar = Evar { eInfo :: Text }
  deriving (Eq, Show)


-- | Suggest some tactics applicable to the current goal.
data Hints = Hints
  deriving (Eq, Show)

-- | 'Hints' response.
data HintsResp =
    HintsNotInProof
  | HintsResp {
      -- | Hints for each hypothesis, in order
      hHypHints :: [[Hint]],
      -- | Hints for the conclusion
      hConcHint :: [Hint]
    }
  deriving (Eq, Show)

data Hint =
    Hint {
      hTactic :: Text,
      -- | TODO: ???
      hAppearance :: Text
    }
  deriving (Eq, Show)


-- | Search for objects satisfying some flags.
data Search = Search { sFlags :: [SearchFlag] }
  deriving (Eq, Show)

data SearchFlag =
    SearchFlag { sfConstraint :: SearchConstraint, sfNegate :: Bool }
  deriving (Eq, Show)

data SearchConstraint =
    -- | Filter by object name
    NamePattern Text
    -- | Filter by object type
  | TypePattern Text
    -- | TODO: ?
  | SubTypePattern Text
    -- | Filter by module path
  | InModule [Text]
    -- | Include blacklisted elements in results
  | IncludeBlacklist
  deriving (Eq, Show)

-- | 'Search' response.
newtype SearchResp =
    SearchResp { srResults :: [CoqObject Text] }
  deriving (Eq, Show)

data CoqObject a =
    CoqObject {
      -- | Rest of module prefix not in 'qualId'
      coPrefix :: [Text],
      -- | Shortest unambiguous name
      coQualId :: [Text],
      coObject :: a
    }
  deriving (Eq, Show)


-- | Get the current values of all options.
data GetOptions = GetOptions deriving (Eq, Show)

-- | 'GetOptions' response.
newtype GetOptionsResp =
    GetOptionsResp { goOptions :: [Option] }
  deriving (Eq, Show)

data Option = Option { oName :: [Text], oState :: OptionState }
  deriving (Eq, Show)

data OptionState =
    OptionState {
      osSynchronous, osDeprecated :: Bool,
      osName                      :: [Text],
      osValue                     :: OptionValue
    }
  deriving (Eq, Show)

data OptionValue =
    BoolValue Bool
  | IntValue (Maybe Int)
  | StringValue Text
  | StringOptValue (Maybe Text)
  deriving (Eq, Show)


-- | Set the value of some options.
newtype SetOptions = SetOptions { soOptions :: [SetOption] }
  deriving (Eq, Show)

data SetOption = SetOption { soName :: [Text], soValue :: OptionValue }
  deriving (Eq, Show)

-- | 'SetOptions' response.
data SetOptionsResp = SetOptionsResp
  deriving (Eq, Show)


-- | Generate a 'match' expression for a given inductive type.
newtype MakeCases = MakeCases { mcTypeName :: Text }
  deriving (Eq, Show)

newtype MakeCasesResp =
    MakeCasesResp {
      -- | A list of arms; each arm is a list of the constructor name
      -- followed by variable names
      mcrCases :: [[Text]]
    }
  deriving (Eq, Show)


-- | Quit @coqtop@.
data Quit = Quit deriving (Eq, Show)

-- | 'Quit' response.
data QuitResp = QuitResp deriving (Eq, Show)
