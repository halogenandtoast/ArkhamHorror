module Arkham.Asset.Cards.LivreDeibon
  ( livreDeibon
  , LivreDeibon(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.Target

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
      $ FastAbility
      $ ExhaustCost
      $ toTarget a
    , withTooltip
        "{fast} Exhaust Livre d'Eibon: Commit the top card of your deck to an eligible skill test performed by an investigator at your location."
      $ restrictedAbility
          a
          2
          (ControlsThis
          <> DuringSkillTest SkillTestAtYourLocation
          <> ExtendedCardExists (TopOfDeckOf You <> EligibleForCurrentSkillTest)
          )
      $ FastAbility
      $ ExhaustCost
      $ toTarget a
    ]

instance RunMessage LivreDeibon where
  runMessage msg a@(LivreDeibon attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      handCards <- field InvestigatorHand iid
      a <$ push
        (chooseOne iid
        $ [ TargetLabel
              (CardIdTarget $ toCardId c)
              [DrawCards iid 1 False, PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) (toCard c)]
          | c <- mapMaybe (preview _PlayerCard) handCards
          ]
        )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      deckCards <- fieldMap InvestigatorDeck unDeck iid
      case deckCards of
        [] -> error "Missing deck card"
        x : _ -> push (SkillTestCommitCard iid $ PlayerCard x)
      pure a
    _ -> LivreDeibon <$> runMessage msg attrs
