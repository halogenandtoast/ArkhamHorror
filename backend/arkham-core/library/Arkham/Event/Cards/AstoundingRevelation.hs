module Arkham.Event.Cards.AstoundingRevelation (
  astoundingRevelation,
  AstoundingRevelation (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Uses (UseType (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Trait

newtype AstoundingRevelation = AstoundingRevelation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

astoundingRevelation :: EventCard AstoundingRevelation
astoundingRevelation = event AstoundingRevelation Cards.astoundingRevelation

instance HasAbilities AstoundingRevelation where
  getAbilities (AstoundingRevelation x) =
    [ playerLimit (PerSearch Research)
        $ mkAbility x 1
        $ ReactionAbility (AmongSearchedCards You) (DiscardCost FromDeck $ SearchedCardTarget $ toCardId x)
    ]

instance RunMessage AstoundingRevelation where
  runMessage msg e@(AstoundingRevelation attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      secretAssets <- selectList $ assetControlledBy iid <> AssetWithUseType Secret
      player <- getPlayer iid
      push
        $ chooseOne player
        $ ResourceLabel iid [TakeResources iid 2 (toAbilitySource attrs 1) False]
        : [targetLabel aid [AddUses aid Secret 1] | aid <- secretAssets]
      pure e
    _ -> AstoundingRevelation <$> runMessage msg attrs
