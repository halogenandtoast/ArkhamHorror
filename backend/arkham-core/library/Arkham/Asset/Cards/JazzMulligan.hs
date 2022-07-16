module Arkham.Asset.Cards.JazzMulligan
  ( jazzMulligan
  , JazzMulligan(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait

newtype JazzMulligan = JazzMulligan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jazzMulligan :: AssetCard JazzMulligan
jazzMulligan = allyWith
  JazzMulligan
  Cards.jazzMulligan
  (2, 2)
  ((isStoryL .~ True) . (slotsL .~ mempty))

instance HasAbilities JazzMulligan where
  getAbilities (JazzMulligan x) =
    [ restrictedAbility x 1 (Unowned <> OnSameLocation)
        $ ActionAbility (Just Parley)
        $ ActionCost 1
    ]

instance HasModifiersFor JazzMulligan where
  getModifiersFor _ (LocationTarget _) (JazzMulligan attrs) = do
    miid <- selectOne You
    case miid of
      Just iid | controlledBy attrs iid ->
        pure [ toModifier attrs (TraitRestrictedModifier Miskatonic Blank)]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage JazzMulligan where
  runMessage msg a@(JazzMulligan attrs@AssetAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      a <$ push (PlaceAsset assetId $ AtLocation lid)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ BeginSkillTest
        iid
        source
        (toTarget attrs)
        (Just Parley)
        SkillIntellect
        3
      pure a
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (TakeControlOfAsset iid assetId)
    _ -> JazzMulligan <$> runMessage msg attrs
