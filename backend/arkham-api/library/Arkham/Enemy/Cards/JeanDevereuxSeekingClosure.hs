module Arkham.Enemy.Cards.JeanDevereuxSeekingClosure (jeanDevereuxSeekingClosure) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.LaidToRest.Helpers

newtype JeanDevereuxSeekingClosure = JeanDevereuxSeekingClosure EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeanDevereuxSeekingClosure :: EnemyCard JeanDevereuxSeekingClosure
jeanDevereuxSeekingClosure =
  enemy JeanDevereuxSeekingClosure Cards.jeanDevereuxSeekingClosure

instance HasModifiersFor JeanDevereuxSeekingClosure where
  getModifiersFor (JeanDevereuxSeekingClosure a) = modifySelf a [CannotBeEngaged]

instance HasAbilities JeanDevereuxSeekingClosure where
  getAbilities (JeanDevereuxSeekingClosure a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "jeanDevereuxSeekingClosure.parley"
          $ restricted
            a
            1
            ( OnSameLocation
                <> exists (EnemyWithTitle "Heretic" <> EnemyAttachedToAsset theBeyond)
                <> exists (locationWithEnemy a.id <> LocationWithCardsUnderneath AnyCards)
                <> exists jimCulver
            )
            parleyAction_
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage JeanDevereuxSeekingClosure where
  runMessage msg e@(JeanDevereuxSeekingClosure attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        cards <- field LocationCardsUnderneath lid
        for_ (headMay cards) \card -> do
          obtainCard card
          for_ (preview _EncounterCard card) \ec -> push $ InvestigatorDrewEncounterCard iid ec
      heretics <- select $ EnemyWithTitle "Heretic" <> EnemyAttachedToAsset theBeyond
      jim <- selectJust jimCulver
      chooseTargetM iid heretics \heretic -> push $ Flip jim (toSource attrs) (toTarget heretic)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      insteadOfDefeatWithWindows attrs do
        healAllDamage (attrs.ability 2) attrs
        flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      possessed <- genCard Cards.jeanDevereuxPossessed
      push $ ReplaceEnemy attrs.id possessed Swap
      pure e
    _ -> JeanDevereuxSeekingClosure <$> liftRunMessage msg attrs
