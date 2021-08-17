module Arkham.Types.Location.Cards.EasttownArkhamPoliceStation
  ( EasttownArkhamPoliceStation(..)
  , easttownArkhamPoliceStation
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (easttownArkhamPoliceStation)
import Arkham.Types.Ability
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype EasttownArkhamPoliceStation = EasttownArkhamPoliceStation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttownArkhamPoliceStation :: LocationCard EasttownArkhamPoliceStation
easttownArkhamPoliceStation = location
  EasttownArkhamPoliceStation
  Cards.easttownArkhamPoliceStation
  4
  (PerPlayer 2)
  Moon
  [Circle, Triangle]

instance HasModifiersFor env EasttownArkhamPoliceStation where
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasAbilities env EasttownArkhamPoliceStation where
  getAbilities iid window@(Window Timing.When NonFast) (EasttownArkhamPoliceStation attrs)
    | locationRevealed attrs
    = withBaseActions iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window (EasttownArkhamPoliceStation attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env EasttownArkhamPoliceStation where
  runMessage msg l@(EasttownArkhamPoliceStation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      ammoAssets <- map (Ammo, )
        <$> selectList (AssetOwnedBy You <> AssetWithUseType Ammo)
      supplyAssets <- map (Supply, )
        <$> selectList (AssetOwnedBy You <> AssetWithUseType Supply)
      l <$ push
        (chooseOne
          iid
          [ AddUses (AssetTarget assetId) useType' 2
          | (useType', assetId) <- ammoAssets <> supplyAssets
          ]
        )
    _ -> EasttownArkhamPoliceStation <$> runMessage msg attrs
