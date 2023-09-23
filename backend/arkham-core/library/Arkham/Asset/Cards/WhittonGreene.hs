module Arkham.Asset.Cards.WhittonGreene (
  whittonGreene,
  WhittonGreene (..),
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

newtype WhittonGreene = WhittonGreene AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whittonGreene :: AssetCard WhittonGreene
whittonGreene = ally WhittonGreene Cards.whittonGreene (2, 2)

instance HasAbilities WhittonGreene where
  getAbilities (WhittonGreene x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          ( OrWindowMatcher
              [ Matcher.RevealLocation Timing.After You Anywhere
              , PutLocationIntoPlay Timing.After You Anywhere
              ]
          )
          (ExhaustCost $ toTarget x)
    ]

instance HasModifiersFor WhittonGreene where
  getModifiersFor (InvestigatorTarget iid) (WhittonGreene a)
    | controlledBy a iid = do
        active <-
          selectAny
            ( AssetControlledBy (InvestigatorWithId iid)
                <> AssetOneOf [AssetWithTrait Tome, AssetWithTrait Relic]
            )
        pure $ toModifiers a [SkillModifier SkillIntellect 1 | active]
  getModifiersFor _ _ = pure []

instance RunMessage WhittonGreene where
  runMessage msg a@(WhittonGreene attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              ( Search
                  iid
                  source
                  (InvestigatorTarget iid)
                  [fromTopOfDeck 6]
                  ( CardWithType AssetType
                      <> CardWithOneOf (map CardWithTrait [Tome, Relic])
                  )
                  (DrawFound iid 1)
              )
    _ -> WhittonGreene <$> runMessage msg attrs
