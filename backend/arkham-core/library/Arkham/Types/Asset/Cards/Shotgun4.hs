module Arkham.Types.Asset.Cards.Shotgun4
  ( Shotgun4(..)
  , shotgun4
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target

newtype Shotgun4 = Shotgun4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shotgun4 :: AssetCard Shotgun4
shotgun4 =
  assetWith Shotgun4 Cards.shotgun4
    $ (slotsL .~ [HandSlot, HandSlot])
    . (startingUsesL ?~ Uses Ammo 2)

instance HasModifiersFor env Shotgun4

instance HasActions env Shotgun4 where
  getActions iid _ (Shotgun4 a) | ownedBy a iid = pure
    [ UseAbility
        iid
        (mkAbility
          (toSource a)
          1
          (ActionAbility
            (Just Action.Fight)
            (Costs [ActionCost 1, UseCost (toId a) Ammo 1])
          )
        )
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Shotgun4 where
  runMessage msg a@(Shotgun4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 3)
      , ChooseFightEnemy iid source SkillCombat mempty False
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
