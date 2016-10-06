-- | Data types for the protocol between @coqtop@ and an editor.
module Coq.Protocol
  (StateId (..), EditId (..),
   -- * Messages
   -- ** @Init@
   Init (..), InitResp (..),
   -- ** @About@
   About (..), AboutResp (..),
   -- ** @Status@
   Status (..), StatusResp (..),
   -- ** @Add@
   Add (..), AddResp (..),
   -- ** @EditAt@
   EditAt (..), EditAtResp (..),
   -- ** @Query@
   Query (..), QueryResp (..),
   -- ** @Goals@
   Goal (..), GoalResp (..), GoalInfo (..),
   -- ** @Evars@
   Evars (..), EvarsResp (..), Evar (..),
   -- ** @Hints@
   Hints (..), HintsResp (..), Hint (..),
   -- ** @Search@
   Search (..), SearchResp (..),
   SearchFlag (..), SearchConstraint (..), CoqObject (..),
   -- ** @GetOptions@
   GetOptions (..), GetOptionsResp (..),
   Option (..), OptionState (..), OptionValue (..),
   -- ** @SetOptions@
   SetOptions (..), SetOptionsResp (..), SetOption (..),
   -- ** @MakeCases@
   MakeCases (..), MakeCasesResp (..),
   -- ** @Quit@
   Quit (..), QuitResp (..),
   -- * Feedback
   Feedback (..), FeedbackContent (..), Location (..),
   RouteId (..),
   OldMessage (..), OldMessageLevel (..))
where

import Data.Text (Text)
import Coq.XmlAst


-- | An identifier for a particular @coqtop@ state.
newtype StateId = StateId Int deriving (Eq, Show)

-- | TODO: ?
newtype EditId = EditId Int deriving (Eq, Show)


data Location = Location { locStart, locEnd :: Int } deriving (Eq, Show)


-- | Load a file or start an empty session.
--
-- If loading an existing file, then 'Init' also adds its directory to the
-- load path if necessary, and loads compilation hints for its filename.
--
-- @
-- Init: string option -> Stateid.t
-- @
data Init =
    Init {
      -- | 'Nothing' to begin an empty session.
      iFilename :: Maybe Text
    }
  deriving (Eq, Show)

-- | 'Init' response.
--
-- @
-- Init : string option -> Stateid.t
-- @
data InitResp =
    InitResp {
      -- State ID for the start of the file
      irStateId :: StateId
    }
  deriving (Eq, Show)


-- | Ask for information about the @coqtop@ executable itself.
--
-- @
-- About : unit -> Interface.coq_info
-- @
data About = About deriving (Eq, Show)

-- | 'About' response.
--
-- @
-- About : unit -> Interface.coq_info
-- @
data AboutResp =
    AboutResp {
      arCoqtopVersion, arProtocolVersion,
        arReleaseDate, arCompileDate :: Text
    }
  deriving (Eq, Show)


-- | Current prover status.
--
-- @
-- Status : bool -> Interface.status
-- @
newtype Status =
    Status {
      -- | Whether to first force evaluation of any unevaluated statements
      sForceEval :: Bool
    }
  deriving (Eq, Show)

-- | 'Status' response.
--
-- @
-- Status : bool -> Interface.status
-- @
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
--
-- @
-- "Add" : ((string * int) * (Stateid.t * bool))
--   -> (Stateid.t * (((unit, Stateid.t) CSig.union) * string))
-- @
data Add =
    Add {
      aPhrase  :: Text,
      -- | TODO: ???
      aEditId  :: EditId,
      -- | Expected current state
      aStateId :: StateId,
      aVerbose :: Bool
    }
  deriving (Eq, Show)

-- | 'Add' response.
--
-- @
-- "Add" : ((string * int) * (Stateid.t * bool))
--   -> (Stateid.t * (((unit, Stateid.t) CSig.union) * string))
-- @
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
--
-- @
-- "Edit_at" :  Stateid.t
--    -> ((unit, (Stateid.t * (Stateid.t * Stateid.t))) CSig.union)
-- @
newtype EditAt =
    EditAt {
      -- | State ID of the phrase to rewind to
      eStateId :: StateId
    }
  deriving (Eq, Show)

-- | 'EditAt' response.
--
-- @
-- "Edit_at" :  Stateid.t
--    -> ((unit, (Stateid.t * (Stateid.t * Stateid.t))) CSig.union)
-- @
data EditAtResp =
    -- | Rewound to requested point
    EditAtNewTip
    -- | Rewound to @tip@, which is in a focusable zone delimited by
    -- @start@ and @stop@
  | EditAtFocus { erStart, erStop, erTip :: StateId }
  deriving (Eq, Show)


-- | Execute a query (e.g. @Print@, @Check@, ...).
--
-- @
-- Query : (string * Stateid.t) -> string
-- @
data Query =
    Query {
      qQuery :: Text,
      -- | Expected current state
      qStateId :: StateId
    }
  deriving (Eq, Show)

-- | 'Query' response.
--
-- @
-- Query : (string * Stateid.t) -> string
-- @
newtype QueryResp = QueryResp { qrMessage :: Text }
  deriving (Eq, Show)


-- | Ask for the current goals.
--
-- @
-- Goal : unit -> (Interface.goals option)
-- @
data Goal = Goal
  deriving (Eq, Show)

-- | 'Goal' response.
--
-- @
-- Goal : unit -> (Interface.goals option)
-- @
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
--
-- @
-- "Evars" : unit -> ((Interface.evar list) option)
-- @
data Evars = Evars
  deriving (Eq, Show)

-- | 'Evars' response.
--
-- @
-- "Evars" : unit -> ((Interface.evar list) option)
-- @
data EvarsResp =
    EvarsNotInProof
  | EvarsResp { eEvars :: [Evar] }
  deriving (Eq, Show)

-- | Description of an evar.
newtype Evar = Evar { eInfo :: Text }
  deriving (Eq, Show)


-- | Suggest some tactics applicable to the current goal.
--
-- @
-- Hints : unit
--   -> (((((string * string) list) list) * ((string * string) list)) option)
-- @
data Hints = Hints
  deriving (Eq, Show)

-- | 'Hints' response.
--
-- @
-- Hints : unit
--   -> (((((string * string) list) list) * ((string * string) list)) option)
-- @
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
--
-- @
-- Search : ((Interface.search_constraint * bool) list)
--   -> ((string Interface.coq_object) list)
-- @
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
--
-- @
-- GetOptions : unit -> (((string list) * Interface.option_state) list)
-- @
data GetOptions = GetOptions deriving (Eq, Show)

-- | 'GetOptions' response.
--
-- @
-- GetOptions : unit -> (((string list) * Interface.option_state) list)
-- @
newtype GetOptionsResp =
    GetOptionsResp { goOptions :: [Option] }
  deriving (Eq, Show)

data Option = Option { oName :: [Text], oState :: OptionState }
  deriving (Eq, Show)

data OptionState =
    OptionState {
      osSynchronous, osDeprecated :: Bool,
      osDescription               :: Text,
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
--
-- @
-- MkCases : string -> ((string list) list)
-- @
newtype MakeCases = MakeCases { mcTypeName :: Text }
  deriving (Eq, Show)

-- | 'MakeCases' response.
--
-- @
-- MkCases : string -> ((string list) list)
-- @
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


data Feedback =
    Feedback {
      fId       :: Either EditId StateId,
      fContents :: FeedbackContent,
      fRoute    :: RouteId
    }
  deriving (Eq, Show)

newtype RouteId = RouteId Int
  deriving (Eq, Show)

-- | Feedback messages. Some of the 'Int's may turn into 'EditId's or
-- 'RouteId's if that's what they turn out to be.
--
-- TODO meanings?
data FeedbackContent =
    -- | The document fragment has completed being processed
    Processed
  | Incomplete
  | Complete
  | ErrorMsg Location Text
    -- | The document fragment has started to be processed, by the worker
    -- thread with the given name.
  | ProcessingIn Text
  | InProgress Int
  | WorkerStatus Text Text
  | Goals Location Text
  | AddedAxiom
  | GlobRef Location Text Text Text Text
  | GlobDef Location Text Text Text
    -- | A file dependency has just been spotted (but not processed yet).
    --
    -- * The first field is 'Nothing' for a direct dependency, and
    --   @'Just' f@ for an indirect dependency coming from @f@ (which is
    --   a full path).
    --
    -- * The second is the full path of the depended-upon file.
  | FileDependency (Maybe Text) Text
    -- | A file has finished loading. The first field is the module name
    -- and the second is the full path to the file.
  | FileLoaded Text Text
  | Custom Location Text Node
  | Message OldMessage
  deriving (Eq, Show)


-- | Old-style feedback messages.
data OldMessage =
    OldMessage {
      omLevel   :: OldMessageLevel,
      omContent :: Text
    }
  deriving (Eq, Show)

data OldMessageLevel =
    LDebug Text
  | LInfo
  | LNotice
  | LWarning
  | LError
  deriving (Eq, Show)
