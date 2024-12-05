module Arkham.Location.Cards.HistoricalSocietyHistoricalMuseum_132 (
  historicalSocietyHistoricalMuseum_132,
  HistoricalSocietyHistoricalMuseum_132 (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg

newtype HistoricalSocietyHistoricalMuseum_132 = HistoricalSocietyHistoricalMuseum_132 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalMuseum_132
  :: LocationCard HistoricalSocietyHistoricalMuseum_132
historicalSocietyHistoricalMuseum_132 =
  location
    HistoricalSocietyHistoricalMuseum_132
    Cards.historicalSocietyHistoricalMuseum_132
    2
    (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyHistoricalMuseum_132 where
  getModifiersFor (HistoricalSocietyHistoricalMuseum_132 a) =
    getSkillTestInvestigator >>= \case
      Nothing -> pure mempty
      Just iid -> maybeModified_ a iid do
        liftGuardM $ isInvestigating iid a.id
        pure [SkillCannotBeIncreased #intellect]

instance HasAbilities HistoricalSocietyHistoricalMuseum_132 where
  getAbilities (HistoricalSocietyHistoricalMuseum_132 attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $ forced $ EnemySpawns #when (be attrs) AnyEnemy
      | attrs.unrevealed
      ]

instance RunMessage HistoricalSocietyHistoricalMuseum_132 where
  runMessage msg l@(HistoricalSocietyHistoricalMuseum_132 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push (Msg.RevealLocation Nothing $ toId attrs)
      pure l
    _ -> HistoricalSocietyHistoricalMuseum_132 <$> liftRunMessage msg attrs
