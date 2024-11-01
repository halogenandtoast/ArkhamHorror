module Arkham.Asset.Assets.LivreDeibon (livreDeibon, LivreDeibon (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype LivreDeibon = LivreDeibon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livreDeibon :: AssetCard LivreDeibon
livreDeibon = asset LivreDeibon Cards.livreDeibon

instance HasAbilities LivreDeibon where
  getAbilities (LivreDeibon a) =
    [ withTooltip
        "{fast} Exhaust Livre d'Eibon: Swap the top card of your deck with a card in your hand."
        $ controlledAbility a 1 CanManipulateDeck
        $ FastAbility (exhaust a)
    , withTooltip
        "{fast} Exhaust Livre d'Eibon: Commit the top card of your deck to an eligible skill test performed by an investigator at your location."
        $ controlledAbility
          a
          2
          ( DuringSkillTest SkillTestAtYourLocation <> exists (TopOfDeckOf You <> EligibleForCurrentSkillTest)
          )
        $ FastAbility (exhaust a)
    ]

instance RunMessage LivreDeibon where
  runMessage msg a@(LivreDeibon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handCards <- field InvestigatorHand iid
      topOfDeck <- take 1 . unDeck <$> field InvestigatorDeck iid
      chooseTargetM iid (onlyPlayerCards handCards) \c -> do
        push $ PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) (toCard c)
        addToHand iid topOfDeck
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      deckCards <- fieldMap InvestigatorDeck unDeck iid
      case deckCards of
        [] -> error "Missing deck card"
        x : _ -> push $ SkillTestCommitCard iid $ PlayerCard x
      pure a
    _ -> LivreDeibon <$> liftRunMessage msg attrs
