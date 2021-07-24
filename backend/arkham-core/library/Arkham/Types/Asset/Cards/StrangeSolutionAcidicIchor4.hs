module Arkham.Types.Asset.Cards.StrangeSolutionAcidicIchor4
  ( strangeSolutionAcidicIchor4
  , StrangeSolutionAcidicIchor4(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype StrangeSolutionAcidicIchor4 = StrangeSolutionAcidicIchor4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionAcidicIchor4 :: AssetCard StrangeSolutionAcidicIchor4
strangeSolutionAcidicIchor4 = assetWith
  StrangeSolutionAcidicIchor4
  Cards.strangeSolutionAcidicIchor4
  (startingUsesL ?~ Uses Supply 4)

instance HasActions env StrangeSolutionAcidicIchor4 where
  getActions iid NonFast (StrangeSolutionAcidicIchor4 attrs) = pure
    [ assetAction iid attrs 1 (Just Action.Fight)
        $ Costs [ActionCost 1, UseCost (toId attrs) Supply 1]
    ]
  getActions iid window (StrangeSolutionAcidicIchor4 attrs) =
    getActions iid window attrs

instance HasModifiersFor env StrangeSolutionAcidicIchor4 where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) (InvestigatorTarget iid) (StrangeSolutionAcidicIchor4 a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillCombat 6, DamageDealt 2]
  getModifiersFor _ _ _ = pure []

instance
  ( HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env StrangeSolutionAcidicIchor4 where
  runMessage msg a@(StrangeSolutionAcidicIchor4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push (ChooseFightEnemy iid source SkillCombat mempty False)
    _ -> StrangeSolutionAcidicIchor4 <$> runMessage msg attrs
