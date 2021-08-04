module Arkham.Types.Asset.Cards.FortyFiveAutomatic
  ( FortyFiveAutomatic(..)
  , fortyFiveAutomatic
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
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype FortyFiveAutomatic = FortyFiveAutomatic AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic :: AssetCard FortyFiveAutomatic
fortyFiveAutomatic = handWith
  FortyFiveAutomatic
  Cards.fortyFiveAutomatic
  (startingUsesL ?~ Uses Ammo 4)

instance HasModifiersFor env FortyFiveAutomatic

instance HasActions env FortyFiveAutomatic where
  getActions iid NonFast (FortyFiveAutomatic a) | ownedBy a iid = pure
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

instance AssetRunner env => RunMessage env FortyFiveAutomatic where
  runMessage msg a@(FortyFiveAutomatic attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [DamageDealt 1, SkillModifier SkillCombat 1]
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    _ -> FortyFiveAutomatic <$> runMessage msg attrs
