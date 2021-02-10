module Arkham.Types.Location.Cards.EasttownArkhamPoliceStation
  ( EasttownArkhamPoliceStation(..)
  , easttownArkhamPoliceStation
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Uses
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait hiding (Supply)

newtype EasttownArkhamPoliceStation = EasttownArkhamPoliceStation LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttownArkhamPoliceStation :: EasttownArkhamPoliceStation
easttownArkhamPoliceStation = EasttownArkhamPoliceStation
  $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "50027"
    (Name "Easttown" (Just "Arkham Police Station"))
    EncounterSet.ReturnToTheMidnightMasks
    4
    (PerPlayer 2)
    Moon
    [Circle, Triangle]
    [Arkham]

instance HasModifiersFor env EasttownArkhamPoliceStation where
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env EasttownArkhamPoliceStation where
  getActions iid NonFast (EasttownArkhamPoliceStation attrs)
    | locationRevealed attrs = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators attrs
      ]
  getActions iid window (EasttownArkhamPoliceStation attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env EasttownArkhamPoliceStation where
  runMessage msg l@(EasttownArkhamPoliceStation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      ammoAssets <- map (Ammo, ) <$> getSetList (iid, Ammo)
      supplyAssets <- map (Supply, ) <$> getSetList (iid, Supply)
      l <$ unshiftMessage
        (chooseOne
          iid
          [ AddUses (AssetTarget assetId) useType' 2
          | (useType', assetId) <- ammoAssets <> supplyAssets
          ]
        )
    _ -> EasttownArkhamPoliceStation <$> runMessage msg attrs
