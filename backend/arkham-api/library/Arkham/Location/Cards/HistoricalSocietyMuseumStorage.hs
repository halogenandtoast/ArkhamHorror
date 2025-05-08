module Arkham.Location.Cards.HistoricalSocietyMuseumStorage (historicalSocietyMuseumStorage) where

import Arkham.Ability
import Arkham.Message.Lifted.Choose
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait

newtype HistoricalSocietyMuseumStorage = HistoricalSocietyMuseumStorage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyMuseumStorage :: LocationCard HistoricalSocietyMuseumStorage
historicalSocietyMuseumStorage = location HistoricalSocietyMuseumStorage Cards.historicalSocietyMuseumStorage 5 (PerPlayer 2)

instance HasAbilities HistoricalSocietyMuseumStorage where
  getAbilities (HistoricalSocietyMuseumStorage a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ EnemyWithTrait Cultist)
      $ forced
      $ InitiatedSkillTest #when You AnySkillType AnySkillTestValue (WhileInvestigating $ be a)

instance RunMessage HistoricalSocietyMuseumStorage where
  runMessage msg l@(HistoricalSocietyMuseumStorage attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) attrs (ShroudModifier (-3))
        let tokens = oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
        onRevealChaosTokenEffect sid tokens attrs attrs (doStep 1 msg)
      pure l
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      when (attrs.token #clue > 0) do
        chooseSelectM iid (FarthestEnemyFrom iid $ EnemyWithTrait Cultist) \cultist -> do
          moveTokens (attrs.ability 1) attrs cultist #clue 1
      pure l
    _ -> HistoricalSocietyMuseumStorage <$> liftRunMessage msg attrs
