module Arkham.Asset.Cards.Shotgun4
  ( Shotgun4(..)
  , shotgun4
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype Shotgun4 = Shotgun4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shotgun4 :: AssetCard Shotgun4
shotgun4 = asset Shotgun4 Cards.shotgun4

instance HasAbilities Shotgun4 where
  getAbilities (Shotgun4 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility (Just Action.Fight) $ Costs
        [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1]
    ]

instance RunMessage Shotgun4 where
  runMessage msg a@(Shotgun4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 3)
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source
      -> let val = min 1 (max 5 n)
         in
           -- This has to be handled specially for cards like Oops!
           a <$ push
             (CreateWindowModifierEffect
               EffectSkillTestWindow
               (FailedByEffectModifiers $ toModifiers attrs [DamageDealt val])
               source
               (InvestigatorTarget iid)
             )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> a <$ push
        (skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (DamageDealt $ min 1 (max 5 n))
        )
    _ -> Shotgun4 <$> runMessage msg attrs
