module Arkham.Asset.Cards.WhittonGreene2 (
  whittonGreene2,
  WhittonGreene2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card.CardType
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype WhittonGreene2 = WhittonGreene2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whittonGreene2 :: AssetCard WhittonGreene2
whittonGreene2 = ally WhittonGreene2 Cards.whittonGreene2 (2, 3)

instance HasAbilities WhittonGreene2 where
  getAbilities (WhittonGreene2 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          ( OrWindowMatcher
              [ Matcher.RevealLocation Timing.After You Anywhere
              , PutLocationIntoPlay Timing.After You Anywhere
              ]
          )
          (ExhaustCost $ toTarget x)
    ]

instance HasModifiersFor WhittonGreene2 where
  getModifiersFor (InvestigatorTarget iid) (WhittonGreene2 a)
    | controlledBy a iid = do
        active <-
          selectAny
            ( AssetControlledBy (InvestigatorWithId iid)
                <> AssetOneOf [AssetWithTrait Tome, AssetWithTrait Relic]
            )
        pure $ toModifiers a $ do
          guard active
          [SkillModifier SkillWillpower 1, SkillModifier SkillIntellect 1]
  getModifiersFor _ _ = pure []

instance RunMessage WhittonGreene2 where
  runMessage msg a@(WhittonGreene2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ search
          iid
          source
          (InvestigatorTarget iid)
          [fromTopOfDeck 9]
          ( CardWithType AssetType
              <> CardWithOneOf (map CardWithTrait [Tome, Relic])
          )
          (DrawFound iid 1)
      pure a
    _ -> WhittonGreene2 <$> runMessage msg attrs
