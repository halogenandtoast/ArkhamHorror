module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheShadeReaper (theShadeReaper, theShadeReaperEffect) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Effect.Import
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype TheShadeReaper = TheShadeReaper ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadeReaper :: ScarletKeyCard TheShadeReaper
theShadeReaper = key TheShadeReaper Cards.theShadeReaper

instance HasAbilities TheShadeReaper where
  getAbilities (TheShadeReaper a) = case a.bearer of
    InvestigatorTarget iid
      | not a.shifted ->
          if a.stable
            then
              [ restricted
                  a
                  1
                  ( youExist (InvestigatorWithId iid)
                      <> exists (NonEliteEnemy <> EnemyAt (locationWithInvestigator iid))
                  )
                  $ FastAbility Free
              ]
            else
              [restricted a 1 (youExist (InvestigatorWithId iid)) $ FastAbility Free]
    _ -> []

instance RunMessage TheShadeReaper where
  runMessage msg k@(TheShadeReaper attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09680]" Null) k
    CampaignSpecific "shift[09680]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          eachInvestigator \iid -> do
            isTurn <- matches iid TurnInvestigator
            if isTurn
              then loseActions iid attrs 1
              else createCardEffect Cards.theShadeReaper Nothing attrs iid
          withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            withLocationOf iid \loc -> do
              enemies <- select $ enemyAt loc <> NonEliteEnemy
              chooseOneToHandle iid attrs enemies
            flipOver iid attrs
      pure k
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget enemy) -> do
      locations <- select $ connectedFrom $ locationWithEnemy enemy
      chooseOneM iid $ withI18n do
        targets locations $ enemyMoveTo attrs enemy
        unscoped skip_
      doStep 1 msg
      pure k
    DoStep 1 (HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget enemy)) -> do
      enemies <- select $ EnemyAt (locationWithEnemy enemy) <> not_ (EnemyWithId enemy)
      n <- (+) <$> field EnemyHealthDamage enemy <*> field EnemySanityDamage enemy
      chooseOneM iid $ withI18n do
        targets enemies $ nonAttackEnemyDamage (Just iid) enemy n
        unscoped skip_
      pure k
    _ -> TheShadeReaper <$> liftRunMessage msg attrs

newtype TheShadeReaperEffect = TheShadeReaperEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadeReaperEffect :: EffectArgs -> TheShadeReaperEffect
theShadeReaperEffect = cardEffect TheShadeReaperEffect Cards.theShadeReaper

instance RunMessage TheShadeReaperEffect where
  runMessage msg e@(TheShadeReaperEffect attrs) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      push $ LoseActions iid attrs.source 1
      disableReturn e
    _ -> TheShadeReaperEffect <$> liftRunMessage msg attrs
