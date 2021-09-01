module Arkham.Types.Location.Cards.StepsOfYhagharl
  ( stepsOfYhagharl
  , StepsOfYhagharl(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (stepsOfYhagharl)
import Arkham.Types.Ability
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype StepsOfYhagharl = StepsOfYhagharl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYhagharl :: LocationCard StepsOfYhagharl
stepsOfYhagharl = location
  StepsOfYhagharl
  Cards.stepsOfYhagharl
  3
  (PerPlayer 1)
  Plus
  [Diamond, Moon]

instance HasAbilities env StepsOfYhagharl where
  getAbilities iid window (StepsOfYhagharl attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1
        $ ForcedAbility
        $ Leaves Timing.When You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env StepsOfYhagharl where
  runMessage msg l@(StepsOfYhagharl attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      encounterDiscard <- map unDiscardedEncounterCard <$> getList ()
      let
        mMadnessCard = find (member Madness . toTraits) encounterDiscard
        revelationMsgs = case mMadnessCard of
          Nothing -> []
          Just madnessCard ->
            [ RemoveFromEncounterDiscard madnessCard
            , InvestigatorDrewEncounterCard iid madnessCard
            ]
      pushAll revelationMsgs
      StepsOfYhagharl <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ push
        (BeginSkillTest
          iid
          source
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          2
        )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        replaceMessageMatching
          (\case
            MoveFrom _ iid' lid' -> iid == iid' && lid' == toId attrs
            _ -> False
          )
          (const [])
        replaceMessageMatching
          (\case
            Will (MoveTo _ iid' _) | iid == iid' -> True
            _ -> False
          )
          (const [])
        replaceMessageMatching
          (\case
            After (Move _ iid' _ _) | iid == iid' -> True
            _ -> False
          )
          (const [])
        l <$ replaceMessageMatching
          (\case
            MoveTo _ iid' _ | iid == iid' -> True
            _ -> False
          )
          (const [ShuffleBackIntoEncounterDeck $ toTarget attrs])
    _ -> StepsOfYhagharl <$> runMessage msg attrs
