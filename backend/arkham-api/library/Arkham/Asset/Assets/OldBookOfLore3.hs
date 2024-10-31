module Arkham.Asset.Assets.OldBookOfLore3 (OldBookOfLore3 (..), oldBookOfLore3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Deck qualified as Deck
import Arkham.Game.Helpers hiding (reduceCostOf)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Strategy
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype OldBookOfLore3 = OldBookOfLore3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore3 :: AssetCard OldBookOfLore3
oldBookOfLore3 = asset OldBookOfLore3 Cards.oldBookOfLore3

instance HasAbilities OldBookOfLore3 where
  getAbilities (OldBookOfLore3 a) =
    [ controlledAbility a 1 (exists $ affectsOthers $ InvestigatorAt YourLocation <> can.search.deck)
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage OldBookOfLore3 where
  runMessage msg a@(OldBookOfLore3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ affectsOthers $ colocatedWith iid
      chooseOrRunOneM iid do
        targets investigators \iid' -> do
          search iid' (attrs.ability 1) iid' [fromTopOfDeck 3] #any (defer attrs IsDraw)
      pure a
    SearchFound _ (isTarget attrs -> True) (Deck.InvestigatorDeck iid') targetCards -> do
      -- TODO after search is an entity we can fix this as this is gross
      let source = attrs.ability 1
      additionalTargets <- getAdditionalSearchTargets iid'
      chooseNM iid' (min (length targetCards) (1 + additionalTargets)) do
        targets targetCards (handleTarget iid' source)
      doStep 1 msg
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      push $ DrawFocusedToHand iid (toTarget iid) FromDeck cid
      pure a
    DoStep
      1
      (SearchFound iid target@(isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') targetCards) -> do
        -- cards that are in hand but also target cards are the ones we can play
        -- if we play a card we remove it from the target cards and recurse
        hand <- field InvestigatorHand iid'
        let cards = filter (`elem` hand) targetCards
        let source = attrs.ability 1
        let windows' = [mkWhen (Window.DuringTurn iid)]

        when (hasUses attrs) $ do
          choices <- forMaybeM cards \card -> do
            resources <- (+ 2) <$> getSpendableResources iid'
            playable <- getIsPlayableWithResources iid' source resources (UnpaidCost NoAction) windows' card
            pure $ guard playable $> card
          when (notNull choices) do
            chooseOneM iid do
              labeled "Do not spend any secrets to play any cards" nothing
              targets choices \card -> do
                push $ SpendUses source (toTarget attrs) Secret 1
                reduceCostOf attrs card 2
                playCardPayingCost iid' card
                doStep 1 $ SearchFound iid target deck (deleteFirst card targetCards)
        pure a
    _ -> OldBookOfLore3 <$> liftRunMessage msg attrs
