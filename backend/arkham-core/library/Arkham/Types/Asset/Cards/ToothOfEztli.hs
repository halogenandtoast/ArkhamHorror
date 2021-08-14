module Arkham.Types.Asset.Cards.ToothOfEztli
  ( toothOfEztli
  , ToothOfEztli(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype ToothOfEztli = ToothOfEztli AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toothOfEztli :: AssetCard ToothOfEztli
toothOfEztli = accessory ToothOfEztli Cards.toothOfEztli

instance HasModifiersFor env ToothOfEztli where
  getModifiersFor (SkillTestSource _ _ (TreacherySource _) _ _) (InvestigatorTarget iid) (ToothOfEztli a)
    | ownedBy a iid
    = pure $ toModifiers
      a
      [SkillModifier SkillWillpower 1, SkillModifier SkillAgility 1]
  getModifiersFor _ _ _ = pure []

instance HasActions ToothOfEztli where
  getActions (ToothOfEztli x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility
          (SkillTestResult
            Timing.After
            You
            (SkillTestOnTreachery AnyTreachery)
            (SuccessResult AnyValue)
          )
          ExhaustThis
        )
    ]

instance AssetRunner env => RunMessage env ToothOfEztli where
  runMessage msg a@(ToothOfEztli attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> ToothOfEztli <$> runMessage msg attrs
