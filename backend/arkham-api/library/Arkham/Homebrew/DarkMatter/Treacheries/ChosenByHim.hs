module Arkham.Homebrew.DarkMatter.Treacheries.ChosenByHim (chosenByHim) where

import Arkham.Ability
import Arkham.Asset.Types qualified as Asset
import Arkham.Deck qualified as Deck
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Ally, Item, Spell))
import Arkham.Treachery.Import.Lifted

newtype ChosenByHim = ChosenByHim TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chosenByHim :: TreacheryCard ChosenByHim
chosenByHim = treachery ChosenByHim Cards.chosenByHim

{- | "Revelation - Attach Chosen by Him to an [[Item]], [[Spell]], or [[Ally]]
asset you control that can leave play.
Forced - At the end of your turn: Take 2 direct damage and shuffle the attached
asset back into your deck.
[action]: Remove attached asset from the game."
-}
instance HasAbilities ChosenByHim where
  getAbilities (ChosenByHim a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    , restricted a 2 (InThreatAreaOf You) actionAbility
    ]

eligibleAssets :: InvestigatorId -> AssetMatcher
eligibleAssets iid =
  assetControlledBy iid
    <> mapOneOf AssetWithTrait [Item, Spell, Ally]
    <> DiscardableAsset

instance RunMessage ChosenByHim where
  runMessage msg t@(ChosenByHim attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ eligibleAssets iid
      if null assets
        then toDiscard attrs attrs
        else chooseOrRunOneM iid $ targets assets $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directDamage iid (attrs.ability 1) 2
      for_ attrs.attached \case
        AssetTarget aid -> do
          card <- field Asset.AssetCard aid
          push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
        _ -> pure ()
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ attrs.attached \case
        AssetTarget aid -> push $ RemoveFromGame (AssetTarget aid)
        _ -> pure ()
      pure t
    _ -> ChosenByHim <$> liftRunMessage msg attrs
