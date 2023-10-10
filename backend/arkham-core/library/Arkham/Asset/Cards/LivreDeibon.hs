module Arkham.Asset.Cards.LivreDeibon (
  livreDeibon,
  LivreDeibon (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
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
        $ restrictedAbility a 1 ControlsThis
        $ FastAbility (exhaust a)
    , withTooltip
        "{fast} Exhaust Livre d'Eibon: Commit the top card of your deck to an eligible skill test performed by an investigator at your location."
        $ restrictedAbility
          a
          2
          ( ControlsThis
              <> DuringSkillTest SkillTestAtYourLocation
              <> ExtendedCardExists (TopOfDeckOf You <> EligibleForCurrentSkillTest)
          )
        $ FastAbility (exhaust a)
    ]

instance RunMessage LivreDeibon where
  runMessage msg a@(LivreDeibon attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      handCards <- field InvestigatorHand iid
      drawing <- drawCards iid (toAbilitySource attrs 1) 1
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel (toCardId c) [drawing, PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) (toCard c)]
          | c <- onlyPlayerCards handCards
          ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      deckCards <- fieldMap InvestigatorDeck unDeck iid
      case deckCards of
        [] -> error "Missing deck card"
        x : _ -> push $ SkillTestCommitCard iid $ PlayerCard x
      pure a
    _ -> LivreDeibon <$> runMessage msg attrs
