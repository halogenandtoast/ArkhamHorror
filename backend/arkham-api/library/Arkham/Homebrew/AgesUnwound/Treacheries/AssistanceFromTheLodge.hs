module Arkham.Homebrew.AgesUnwound.Treacheries.AssistanceFromTheLodge (assistanceFromTheLodge) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype AssistanceFromTheLodge = AssistanceFromTheLodge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Set up only if /you have advanced the schemes of the Silver Twilight Lodge/;
otherwise both copies are removed from the game. Surge is on the card def, so
helping the investigators still costs them a card off the encounter deck.
-}
assistanceFromTheLodge :: TreacheryCard AssistanceFromTheLodge
assistanceFromTheLodge = treachery AssistanceFromTheLodge Cards.assistanceFromTheLodge

{- | "Surge. /
Revelation - Take 1 horror. Then, choose one:
- Deal 2 damage to an enemy.
- Discover a clue from your location or a connecting location.
- Heal 2 damage."

"an enemy" is printed with no location qualifier, so any enemy in play is a legal
target --- generous for an encounter card, but the card data is authoritative and
this one is the Lodge doing the investigators a favour.
-}
instance RunMessage AssistanceFromTheLodge where
  runMessage msg t@(AssistanceFromTheLodge attrs) = runQueueT $ campaignI18n $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      -- "Then": the choice is offered after the horror has landed, so an option
      -- that the horror has just taken away is not offered.
      doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      enemies <- select AnyEnemy
      clueSpots <- select $ oneOf [locationWithInvestigator iid, connectedFrom (locationWithInvestigator iid)]
      healable <- selectAny $ HealableInvestigator (toSource attrs) #damage (InvestigatorWithId iid)
      chooseOrRunOneM iid do
        unless (null enemies) do
          labeled "assistanceFromTheLodge.dealDamage" $ chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) attrs 2
        unless (null clueSpots) do
          labeled "assistanceFromTheLodge.discoverClue"
            $ discoverAtMatchingLocation_
              iid
              attrs
              (oneOf [locationWithInvestigator iid, connectedFrom (locationWithInvestigator iid)])
              1
        when healable do
          labeled "assistanceFromTheLodge.healDamage" $ healDamage iid attrs 2
      pure t
    _ -> AssistanceFromTheLodge <$> liftRunMessage msg attrs
