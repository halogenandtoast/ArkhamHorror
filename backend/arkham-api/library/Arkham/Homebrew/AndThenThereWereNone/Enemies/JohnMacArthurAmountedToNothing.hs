{- | The General, unmasked. He drains a sigil every night and the party dies the
night one of them runs dry, so evading him is how they refill it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.JohnMacArthurAmountedToNothing (
  johnMacArthurAmountedToNothing,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype JohnMacArthurAmountedToNothing = JohnMacArthurAmountedToNothing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

johnMacArthurAmountedToNothing :: EnemyCard JohnMacArthurAmountedToNothing
johnMacArthurAmountedToNothing =
  enemy JohnMacArthurAmountedToNothing Cards.johnMacArthurAmountedToNothing

instance HasAbilities JohnMacArthurAmountedToNothing where
  getAbilities (JohnMacArthurAmountedToNothing a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "johnMacArthurAmountedToNothing.drain"
          $ mkAbility a 1
          $ forced
          $ PhaseEnds #when #enemy
      , scenarioI18n
          $ withI18nTooltip "johnMacArthurAmountedToNothing.offerings"
          $ mkAbility a 2
          $ freeReaction (EnemyEvaded #after You (be a))
      ]

instance RunMessage JohnMacArthurAmountedToNothing where
  runMessage msg e@(JohnMacArthurAmountedToNothing attrs) = runQueueT $ case msg of
    -- "remove 1 offering from each copy of Antediluvian Sigils."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach antediluvianSigils $ removeTokensOn (attrs.ability 1) #offering 1
      doStep 1 msg
      pure e
    -- "Then, if there are no offerings on a copy of Antediluvian Sigils, each
    -- investigator is defeated and takes a physical trauma."
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      offerings <- offeringsPerSigil
      when (0 `elem` offerings) $ eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        defeat (attrs.ability 1) iid
      pure e
    -- "Add 2 offerings to any copy of Antediluvian Sigils."
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sigils <- select antediluvianSigils
      chooseOrRunOneM iid $ targets sigils \aid -> placeTokens (attrs.ability 2) aid #offering 2
      pure e
    _ -> JohnMacArthurAmountedToNothing <$> liftRunMessage msg attrs
