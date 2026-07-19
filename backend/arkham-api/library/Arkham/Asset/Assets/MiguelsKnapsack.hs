module Arkham.Asset.Assets.MiguelsKnapsack (miguelsKnapsack) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card.CardDef (cdOverrideActionPlayableIfCriteriaMet, toCardDef)
import {-# SOURCE #-} Arkham.Game (withoutCanModifiers)
import Arkham.Helpers.Card (cardPassesLimitsAtLocation)
import Arkham.Helpers.Game (withAlteredGame)
import Arkham.Helpers.Location (getConnectedLocations, withLocationOf)
import Arkham.Helpers.Modifiers (modified_, withModifiersOf)
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype MiguelsKnapsack = MiguelsKnapsack AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miguelsKnapsack :: AssetCard MiguelsKnapsack
miguelsKnapsack = assetWith MiguelsKnapsack Cards.miguelsKnapsack (healthL ?~ 2)

instance HasModifiersFor MiguelsKnapsack where
  getModifiersFor (MiguelsKnapsack a) = unless a.exhausted do
    for_ a.controller \iid ->
      modified_
        a
        iid
        [CanModify $ CanPlayAtLocation #event $ connectedFrom (locationWithInvestigator iid)]

instance HasAbilities MiguelsKnapsack where
  getAbilities (MiguelsKnapsack a) =
    [controlled_ a 1 $ triggered (PlayEvent #when You AnyEvent) (exhaust a)]

instance RunMessage MiguelsKnapsack where
  runMessage msg a@(MiguelsKnapsack attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getPlayedEvent -> eid) _ -> do
      withLocationOf iid \lid -> do
        card <- fetchCard eid
        let
          source = CardIdSource card.id
          enemyMatchers =
            [CanFightEnemy source | #fight `elem` card.actions]
              <> [CanEvadeEnemy source | #evade `elem` card.actions]
          playableAt lid'
            | null enemyMatchers = pure True
            | otherwise =
                withAlteredGame withoutCanModifiers
                  $ withModifiersOf iid source [AsIfAt lid']
                  $ orM [selectAny $ matcher <> at_ (LocationWithId lid') | matcher <- enemyMatchers]
        playableHere <-
          if cdOverrideActionPlayableIfCriteriaMet (toCardDef card)
            then pure True
            else playableAt lid
        validConnectingLids <-
          getConnectedLocations lid >>= filterM \lid' ->
            andM [cardPassesLimitsAtLocation card lid', playableAt lid']
        let
          playAtConnectingLocation :: ReverseQueue m => m ()
          playAtConnectingLocation = chooseOrRunOneM iid do
            targets validConnectingLids \lid' ->
              cardResolutionModifiers card (attrs.ability 1) iid [AsIfAt lid']
        if not playableHere && notNull validConnectingLids
          then playAtConnectingLocation
          else chooseOneM iid do
            (withI18n $ countVar 1 $ labeled' "drawCards") do
              drawCards iid (attrs.ability 1) 1
            when (notNull validConnectingLids) do
              (cardI18n $ labeled' "miguelsKnapsack.playThatEventAtAConnectingLocation") playAtConnectingLocation
      pure a
    _ -> MiguelsKnapsack <$> liftRunMessage msg attrs
