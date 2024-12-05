module Arkham.Location.Cards.HistoricalSocietyHistoricalMuseum_130 (
  historicalSocietyHistoricalMuseum_130,
  HistoricalSocietyHistoricalMuseum_130 (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg

newtype HistoricalSocietyHistoricalMuseum_130 = HistoricalSocietyHistoricalMuseum_130 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalMuseum_130
  :: LocationCard HistoricalSocietyHistoricalMuseum_130
historicalSocietyHistoricalMuseum_130 =
  location
    HistoricalSocietyHistoricalMuseum_130
    Cards.historicalSocietyHistoricalMuseum_130
    2
    (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyHistoricalMuseum_130 where
  getModifiersFor (HistoricalSocietyHistoricalMuseum_130 a) =
    getSkillTestInvestigator >>= \case
      Nothing -> pure mempty
      Just iid -> maybeModified_ a iid do
        liftGuardM $ isInvestigating iid a.id
        pure [SkillCannotBeIncreased #intellect]

instance HasAbilities HistoricalSocietyHistoricalMuseum_130 where
  getAbilities (HistoricalSocietyHistoricalMuseum_130 attrs) =
    extend
      attrs
      [ mkAbility attrs 1 $ forced $ EnemySpawns #when (be attrs) AnyEnemy
      | attrs.unrevealed
      ]

instance RunMessage HistoricalSocietyHistoricalMuseum_130 where
  runMessage msg l@(HistoricalSocietyHistoricalMuseum_130 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> HistoricalSocietyHistoricalMuseum_130 <$> liftRunMessage msg attrs
