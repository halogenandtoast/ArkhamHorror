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
import Arkham.Helpers
import Arkham.Investigator.Attrs ( Field (..) )
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
    [ restrictedAbility a 1 OwnsThis $ FastAbility $ ExhaustCost $ toTarget a
    , restrictedAbility
        a
        2
        (OwnsThis
        <> DuringSkillTest SkillTestAtYourLocation
        <> ExtendedCardExists (TopOfDeckOf You <> EligibleForCurrentSkillTest)
        )
      $ FastAbility
      $ ExhaustCost
      $ toTarget a
    ]

instance RunMessage LivreDeibon where
  runMessage msg a@(LivreDeibon attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      handCards <- field InvestigatorHand iid
      a <$ push
        (chooseOne iid
        $ [ TargetLabel
              (CardIdTarget $ toCardId c)
              [DrawCards iid 1 False, PutOnTopOfDeck iid c]
          | c <- mapMaybe (preview _PlayerCard) handCards
          ]
        )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      deckCards <- fieldMap InvestigatorDeck unDeck iid
      case deckCards of
        [] -> error "Missing deck card"
        x : _ -> push (SkillTestCommitCard iid (toCardId x))
      pure a
    _ -> LivreDeibon <$> runMessage msg attrs
