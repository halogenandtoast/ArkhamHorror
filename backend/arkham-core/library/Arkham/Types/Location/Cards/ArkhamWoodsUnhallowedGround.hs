module Arkham.Types.Location.Cards.ArkhamWoodsUnhallowedGround where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsUnhallowedGround :: ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround $ base
  { locationRevealedConnectedSymbols = setFromList
    [Squiggle, Hourglass, Diamond]
  , locationRevealedSymbol = Triangle
  }
 where
  base = baseAttrs
    "01150"
    (Name "Arkham Woods" (Just "Unhallowed Ground"))
    EncounterSet.TheDevourerBelow
    4
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsUnhallowedGround where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsUnhallowedGround where
  getActions i window (ArkhamWoodsUnhallowedGround attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsUnhallowedGround where
  runMessage msg l@(ArkhamWoodsUnhallowedGround attrs@LocationAttrs {..}) = case msg of
    MoveTo iid lid | lid == locationId -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (LocationSource "01150")
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          4
        )
      ArkhamWoodsUnhallowedGround <$> runMessage msg attrs
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ unshiftMessage
        (InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1)
    _ -> ArkhamWoodsUnhallowedGround <$> runMessage msg attrs
