module Arkham.Types.Asset.Cards.Pickpocketing
  ( Pickpocketing(..)
  , pickpocketing
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
import Arkham.Types.Window

newtype Pickpocketing = Pickpocketing AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing :: AssetCard Pickpocketing
pickpocketing = asset Pickpocketing Cards.pickpocketing

instance HasModifiersFor env Pickpocketing

instance HasAbilities env Pickpocketing where
  getAbilities iid (AfterEnemyEvaded who enemyId) (Pickpocketing a) | iid == who =
    withBaseActions iid (AfterEnemyEvaded who enemyId) a $ do
      let ability = mkAbility a 1 $ LegacyReactionAbility $ ExhaustCost (toTarget a)
      pure [ ability | ownedBy a iid ]
  getAbilities i window (Pickpocketing a) = getAbilities i window a

instance AssetRunner env => RunMessage env Pickpocketing where
  runMessage msg a@(Pickpocketing attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> Pickpocketing <$> runMessage msg attrs
