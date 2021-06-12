module Arkham.Types.Location.Cards.ArkhamWoodsUnhallowedGround where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsUnhallowedGround :: LocationId -> ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround =
  ArkhamWoodsUnhallowedGround
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, Hourglass, Diamond])
    . (revealedSymbolL .~ Triangle)
    . baseAttrs
        "01150"
        ("Arkham Woods" `subtitled` "Unhallowed Ground")
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
  runMessage msg l@(ArkhamWoodsUnhallowedGround attrs@LocationAttrs {..}) =
    case msg of
      MoveTo iid lid | lid == locationId -> do
        unshiftMessage
          (BeginSkillTest
            iid
            (toSource attrs)
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
