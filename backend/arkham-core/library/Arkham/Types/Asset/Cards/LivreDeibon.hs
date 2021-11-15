module Arkham.Types.Asset.Cards.LivreDeibon
  ( livreDeibon
  , LivreDeibon(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Target

newtype LivreDeibon = LivreDeibon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
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
        <> ExtendedCardMatches
             (TopOfDeckOf You <> EligibleForCurrentSkillTest)
        )
      $ FastAbility
      $ ExhaustCost
      $ toTarget a
    ]

instance AssetRunner env => RunMessage env LivreDeibon where
  runMessage msg a@(LivreDeibon attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      handCards <- map unHandCard <$> getList iid
      a <$ push
        (chooseOne iid
        $ [ TargetLabel
              (CardIdTarget $ toCardId c)
              [DrawCards iid 1 False, PutOnTopOfDeck iid c]
          | c <- mapMaybe (preview _PlayerCard) handCards
          ]
        )
    UseCardAbility _ source _ 2 _ | isSource attrs source -> pure a
    _ -> LivreDeibon <$> runMessage msg attrs
