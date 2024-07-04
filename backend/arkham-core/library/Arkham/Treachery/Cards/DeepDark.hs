module Arkham.Treachery.Cards.DeepDark (deepDark, DeepDark (..)) where

import Arkham.Ability
import Arkham.Helpers.Discover
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Data.Map.Strict qualified as Map

newtype Metadata = Metadata {investigatorLocationsClues :: Map InvestigatorId (Set LocationId)}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DeepDark = DeepDark (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepDark :: TreacheryCard DeepDark
deepDark = treachery (DeepDark . (`with` Metadata mempty)) Cards.deepDark

instance HasModifiersFor DeepDark where
  getModifiersFor (LocationTarget _) (DeepDark (a `With` _)) =
    pure $ toModifiers a [MaxCluesDiscovered 1]
  getModifiersFor (InvestigatorTarget iid) (DeepDark (a `With` metadata)) = do
    let lids = setToList $ Map.findWithDefault mempty iid $ investigatorLocationsClues metadata
    modified a $ case lids of
      [] -> []
      xs -> [CannotDiscoverCluesAt $ LocationMatchAny $ map LocationWithId xs]
  getModifiersFor _ _ = pure []

instance HasAbilities DeepDark where
  getAbilities (DeepDark (a `With` _)) =
    [limitedAbility (MaxPer Cards.deepDark PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage DeepDark where
  runMessage msg t@(DeepDark (attrs `With` metadata)) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    DiscoverClues iid discover -> do
      getDiscoverLocation iid discover >>= \case
        Just lid -> do
          -- TODO: We should track this via history instead
          let
            updatedMetadata =
              Map.insertWith
                (<>)
                iid
                (singleton lid)
                (investigatorLocationsClues metadata)
          pure . DeepDark $ attrs `with` (Metadata updatedMetadata)
        Nothing -> pure t
    EndRound -> pure . DeepDark $ attrs `with` (Metadata mempty)
    _ -> DeepDark . (`with` metadata) <$> liftRunMessage msg attrs
