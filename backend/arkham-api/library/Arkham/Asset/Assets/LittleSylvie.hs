module Arkham.Asset.Assets.LittleSylvie (littleSylvie) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (Discarded, EncounterCardSource)
import Arkham.Classes.HasQueue
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.ScenarioLogKey (ScenarioLogKey (LittleSylvieCanBeTakenControl))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype LittleSylvie = LittleSylvie AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

littleSylvie :: AssetCard LittleSylvie
littleSylvie = assetWith LittleSylvie Cards.littleSylvie (sanityL ?~ 1)

instance HasModifiersFor LittleSylvie where
  getModifiersFor _ = pure mempty

instance HasAbilities LittleSylvie where
  getAbilities (LittleSylvie a) =
    [ -- "When Little Sylvie is discarded from your hand or deck: Play it"
      restricted a 1 InYourDiscard
        $ freeReaction
        $ DiscardedFromHand #after You AnySource
        $ PlayableCardWithCriteria NoAction (CriteriaOverride NoRestriction)
        $ basic
        $ CardWithId a.cardId
    , -- "When a scenario card effect would discard a card from your hand or
      -- deck, exhaust Little Sylvie: Place that card on top of your deck
      -- instead." Hand and deck branches share an exhaust + replacement step.
      controlled_ a 2
        $ triggered
          ( oneOf
              [ WouldDiscardFromHand #when You EncounterCardSource
              , WouldDiscardFromDeck #when You EncounterCardSource
              ]
          )
          (exhaust a)
    , -- Granted by Codex 4 (William Hemlock) on Day 1 when no investigator
      -- already controls Sylvie: "[fast]: Take control of Little Sylvie."
      restricted a 3 (Remembered LittleSylvieCanBeTakenControl) $ FastAbility Free
    ]

instance RunMessage LittleSylvie where
  runMessage msg a@(LittleSylvie attrs) = runQueueT $ case msg of
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      payCardCost iid attrs
      putCardIntoPlay iid attrs
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      for_ ws \w -> do
        lift $ case windowType w of
          Window.WouldDiscardFromHand iid' source | iid == iid' -> do
            replaceMessageMatching
              \case
                Do (DiscardFromHand handDiscard) ->
                  handDiscard.investigator == iid && handDiscard.source == toSource source
                _ -> False
              \case
                Do (DiscardFromHand _) -> [DoStep 1 msg]
                _ -> []
          Window.WouldDiscardFromDeck iid' source | iid == iid' -> do
            -- Cancel the upcoming `Do (DiscardTopOfDeck ...)` (the top card
            -- "stays on top of the deck", so the discard becomes a no-op).
            replaceMessageMatching
              \case
                Do (DiscardTopOfDeck iid'' _ source' _) ->
                  iid'' == iid && source' == toSource source
                _ -> False
              (const [])
          _ -> pure ()
      pure a
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 2 _ _) -> do
      hand <- field InvestigatorHand iid
      case hand of
        [] -> pure ()
        cards -> chooseTargetM iid cards \c -> putCardOnTopOfDeck iid iid c
      pure a
    UseCardAbility iid (isSource attrs -> True) 3 _ _ -> do
      takeControlOfAsset iid attrs
      pure a
    _ -> LittleSylvie <$> liftRunMessage msg attrs
