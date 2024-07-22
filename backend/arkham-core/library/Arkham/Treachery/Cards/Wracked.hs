module Arkham.Treachery.Cards.Wracked (wracked, Wracked (..)) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestSource)
import Arkham.History
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Wracked = Wracked TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wracked :: TreacheryCard Wracked
wracked = treachery Wracked Cards.wracked

instance HasModifiersFor Wracked where
  getModifiersFor (InvestigatorTarget iid) (Wracked attrs) = do
    mSource <- getSkillTestSource
    mInvestigator <- getSkillTestInvestigator
    case (mSource, mInvestigator) of
      (Just source, Just iid') | iid == iid' -> do
        performed <- historySkillTestsPerformed <$> getHistory RoundHistory iid
        hasExhaustedWitch <- selectAny $ ExhaustedEnemy <> EnemyWithTrait Witch <> enemyAtLocationWith iid
        pure
          $ toModifiers attrs
          $ [AnySkillValue (-1) | null performed && treacheryInThreatArea iid attrs]
          <> [ SkillTestAutomaticallySucceeds
             | hasExhaustedWitch && isSource attrs source
             ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Wracked where
  getAbilities (Wracked a) = [restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage Wracked where
  runMessage msg t@(Wracked attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Wracked <$> liftRunMessage msg attrs
