module Arkham.Asset.Assets.LittleSylvie (littleSylvie) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (Discarded, EncounterCardSource)
import Arkham.Classes.HasQueue
import Arkham.Discard
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype LittleSylvie = LittleSylvie AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

littleSylvie :: AssetCard LittleSylvie
littleSylvie = assetWith LittleSylvie Cards.littleSylvie (sanityL ?~ 1)

instance HasAbilities LittleSylvie where
  getAbilities (LittleSylvie a) =
    [ restricted a 1 InYourDiscard
        $ freeReaction
        $ oneOf
          [ DiscardedFromHand #after You AnySource
              $ PlayableCardWithCriteria NoAction (CriteriaOverride NoRestriction)
              $ basic
              $ CardWithId a.cardId
          , DiscardedFromDeck #after You AnySource
              $ PlayableCardWithCriteria NoAction (CriteriaOverride NoRestriction)
              $ basic
              $ CardWithId a.cardId
          ]
    , controlled_ a 2
        $ triggered
          ( oneOf
              [ WouldDiscardFromHand #when You EncounterCardSource
              , WouldDiscardFromDeck #when You EncounterCardSource
              ]
          )
          (exhaust a)
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
                Do (DiscardFromHand handDiscard) ->
                  [Do (DiscardFromHand handDiscard {discardDestination = ToTopOfDeck})]
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
    UseCardAbility iid (isSource attrs -> True) 3 _ _ -> do
      takeControlOfAsset iid attrs
      pure a
    _ -> LittleSylvie <$> liftRunMessage msg attrs
