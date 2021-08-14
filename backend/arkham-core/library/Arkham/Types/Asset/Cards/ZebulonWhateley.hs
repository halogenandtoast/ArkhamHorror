module Arkham.Types.Asset.Cards.ZebulonWhateley
  ( zebulonWhateley
  , ZebulonWhateley(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype ZebulonWhateley = ZebulonWhateley AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zebulonWhateley :: AssetCard ZebulonWhateley
zebulonWhateley =
  allyWith ZebulonWhateley Cards.zebulonWhateley (1, 4) (isStoryL .~ True)

instance HasActions ZebulonWhateley where
  getActions (ZebulonWhateley x) =
    [ restrictedAbility x 1 OwnsThis $ ReactionAbility
        (SkillTestResult
          Timing.After
          You
          (SkillTestOnTreachery AnyTreachery)
          (SuccessResult AnyValue)
        )
        ExhaustThis
    ]

instance HasModifiersFor env ZebulonWhateley where
  getModifiersFor _ (InvestigatorTarget iid) (ZebulonWhateley a) =
    pure [ toModifier a (SkillModifier SkillWillpower 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env ZebulonWhateley where
  runMessage msg a@(ZebulonWhateley attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> ZebulonWhateley <$> runMessage msg attrs
