module Arkham.Location.Cards.TheBlobThatAteEverythingELSE.AbandonedWindmill (abandonedWindmill) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Modifiers (ModifierType (AlternateSuccessfullInvestigation))
import Arkham.Location.CardDefs.TheBlobThatAteEverything qualified as Cards
import Arkham.Location.CardDefs.TheBlobThatAteEverythingELSE qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AbandonedWindmill = AbandonedWindmill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedWindmill :: LocationCard AbandonedWindmill
abandonedWindmill = locationWith AbandonedWindmill Cards.abandonedWindmill 5 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AbandonedWindmill where
  getAbilities (AbandonedWindmill a) =
    extendRevealed1 a $ restricted a 1 (Here <> thisExists a InvestigatableLocation) investigateAction_

instance RunMessage AbandonedWindmill where
  runMessage msg l@(AbandonedWindmill attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier
        sid
        (attrs.ability 1)
        attrs
        (AlternateSuccessfullInvestigation $ ProxyTarget (toTarget attrs) (toTarget attrs))
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 1)
      pure l
    Successful (Action.Investigate, _) iid _ (ProxyTarget (isTarget attrs -> True) _) clueCount -> do
      destinations <-
        select
          $ oneOf
            [ locationIs Cards.researchSite
            , locationIs Cards.temporaryHQ
            ]
      chooseOrRunOneM iid $ targets destinations \loc -> placeClues (attrs.ability 1) loc clueCount
      pure l
    _ -> AbandonedWindmill <$> liftRunMessage msg attrs
