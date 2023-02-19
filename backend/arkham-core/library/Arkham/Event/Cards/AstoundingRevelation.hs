module Arkham.Event.Cards.AstoundingRevelation
  ( astoundingRevelation
  , AstoundingRevelation(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Uses ( UseType (..) )
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype AstoundingRevelation = AstoundingRevelation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astoundingRevelation :: EventCard AstoundingRevelation
astoundingRevelation = event AstoundingRevelation Cards.astoundingRevelation

instance HasAbilities AstoundingRevelation where
  getAbilities (AstoundingRevelation x) =
    [ limitedAbility (PlayerLimit (PerSearch Research) 1)
        $ mkAbility x 1
        $ ReactionAbility
            (AmongSearchedCards You)
            (DiscardCost FromDeck $ SearchedCardTarget $ toCardId x)
    ]

instance RunMessage AstoundingRevelation where
  runMessage msg e@(AstoundingRevelation attrs) = case msg of
    InDiscard _ (UseCardAbility iid source 1 _ _) | isSource attrs source -> do
      secretAssetIds <- selectList
        (AssetControlledBy You <> AssetWithUseType Secret)
      push
        $ chooseOne iid
        $ ComponentLabel
            (InvestigatorComponent iid ResourceToken)
            [TakeResources iid 2 (toAbilitySource attrs 1) False]
        : [ targetLabel aid [AddUses aid Secret 1]
          | aid <- secretAssetIds
          ]
      pure e
    _ -> AstoundingRevelation <$> runMessage msg attrs
