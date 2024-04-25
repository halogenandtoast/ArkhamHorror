module Arkham.Asset.Cards.OldBookOfLore3 (
  OldBookOfLore3 (..),
  oldBookOfLore3,
  oldBookOfLore3Effect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
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
  runMessage msg a@(OldBookOfLore3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ affectsOthers $ colocatedWith iid
      let source = toAbilitySource attrs 1
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ targetLabels investigators \iid' -> do
          only $ search iid' source iid' [fromTopOfDeck 3] AnyCard (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound _ (isTarget attrs -> True) (Deck.InvestigatorDeck iid') targetCards -> do
      -- TODO after search is an entity we can fix this as this is gross
      push (DoStep 1 msg)
      let source = toAbilitySource attrs 1
      additionalTargets <- getAdditionalSearchTargets iid'
      player <- getPlayer iid'
      push
        $ chooseN
          player
          (min (length targetCards) (1 + additionalTargets))
          [ targetLabel (toCardId card) [HandleTargetChoice iid' source (CardIdTarget $ toCardId card)]
          | card <- targetCards
          ]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      push $ AddFocusedToHand iid (toTarget iid) FromDeck cid
      pure a
    DoStep
      1
      (SearchFound iid target@(isTarget attrs -> True) deck@(Deck.InvestigatorDeck iid') targetCards) -> do
        -- cards that are in hand but also target cards are the ones we can play
        -- if we play a card we remove it from the target cards and recurse
        hand <- field InvestigatorHand iid'
        let cards = filter (`elem` hand) targetCards
        let windows' = [mkWhen (Window.DuringTurn iid)]
        let source = toAbilitySource attrs 1

        when (hasUses attrs) $ do
          choices <- forMaybeM cards \card -> do
            spendableResources <- (+ 2) <$> getSpendableResources iid'
            playable <-
              getIsPlayableWithResources iid' source spendableResources (UnpaidCost NoAction) windows' card
            pure
              $ guard playable
              $> targetLabel
                (toCardId card)
                [ SpendUses (toTarget attrs) Secret 1
                , createCardEffect Cards.oldBookOfLore3 Nothing attrs (toCardId card)
                , PayCardCost iid' card windows'
                , DoStep 1 (SearchFound iid target deck (deleteFirst card targetCards))
                ]

          player <- getPlayer iid
          pushIfAny choices
            $ chooseOne player (Label "Do not spend any secrets to play any cards" [] : choices)

        pure a
    _ -> OldBookOfLore3 <$> runMessage msg attrs

newtype OldBookOfLore3Effect = OldBookOfLore3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore3Effect :: EffectArgs -> OldBookOfLore3Effect
oldBookOfLore3Effect = cardEffect OldBookOfLore3Effect Cards.oldBookOfLore3

instance HasModifiersFor OldBookOfLore3Effect where
  getModifiersFor target@(CardIdTarget cid) (OldBookOfLore3Effect attrs) | attrs.target `is` target = do
    pure $ toModifiers attrs [ReduceCostOf (CardWithId cid) 2]
  getModifiersFor _ _ = pure []

instance RunMessage OldBookOfLore3Effect where
  runMessage msg e@(OldBookOfLore3Effect attrs) = case msg of
    ResolvedCard _ card | CardIdTarget (toCardId card) == attrs.target -> do
      push $ disable attrs
      pure e
    _ -> OldBookOfLore3Effect <$> runMessage msg attrs
