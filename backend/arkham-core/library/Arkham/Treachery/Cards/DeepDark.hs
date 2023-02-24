module Arkham.Treachery.Cards.DeepDark
  ( deepDark
  , DeepDark(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher hiding ( DiscoverClues )
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Data.HashMap.Strict qualified as HashMap

newtype Metadata = Metadata { investigatorLocationsClues :: HashMap InvestigatorId (HashSet LocationId) }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DeepDark = DeepDark (TreacheryAttrs `With` Metadata)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepDark :: TreacheryCard DeepDark
deepDark = treachery (DeepDark . (`with` Metadata mempty)) Cards.deepDark

instance HasModifiersFor DeepDark where
  getModifiersFor (LocationTarget _) (DeepDark (a `With` _)) =
    pure $ toModifiers a [MaxCluesDiscovered 1]
  getModifiersFor (InvestigatorTarget iid) (DeepDark (a `With` metadata)) = do
    let
      lids =
        setToList
          $ HashMap.findWithDefault mempty iid
          $ investigatorLocationsClues metadata
    case lids of
      [] -> pure []
      xs -> pure $ toModifiers
        a
        [CannotDiscoverCluesAt $ LocationMatchAny $ map LocationWithId xs]
  getModifiersFor _ _ = pure []

instance HasAbilities DeepDark where
  getAbilities (DeepDark (a `With` _)) =
    [ limitedAbility (PerCopyLimit Cards.deepDark PerRound 1)
        $ mkAbility a 1
        $ ForcedAbility
        $ RoundEnds Timing.When
    ]

instance RunMessage DeepDark where
  runMessage msg t@(DeepDark (attrs `With` metadata)) = case msg of
    Revelation _iid source | isSource attrs source -> do
      push $ PlaceTreachery (toId attrs) TreacheryNextToAct
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    DiscoverClues iid lid _ _ -> do
      let
        updatedMetadata = HashMap.insertWith
          (<>)
          iid
          (singleton lid)
          (investigatorLocationsClues metadata)
      pure . DeepDark $ attrs `with` (Metadata updatedMetadata)
    EndRound -> pure . DeepDark $ attrs `with` (Metadata mempty)
    _ -> DeepDark . (`with` metadata) <$> runMessage msg attrs
