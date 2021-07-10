module Arkham.Types.Asset.Cards.ArcaneInitiate where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype ArcaneInitiate = ArcaneInitiate AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate :: AssetCard ArcaneInitiate
arcaneInitiate = ally ArcaneInitiate Cards.arcaneInitiate (1, 2)

instance HasModifiersFor env ArcaneInitiate

instance HasAbilities ArcaneInitiate where
  getAbilities (ArcaneInitiate a) =
    [assetAbility a 1 . FreeAbility Nothing . ExhaustCost $ toTarget a]

instance (AssetRunner env) => RunMessage env ArcaneInitiate where
  runMessage msg a@(ArcaneInitiate attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ArcaneInitiate <$> runMessage msg (attrs & doomL +~ 1)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ SearchTopOfDeck
            iid
            source
            (InvestigatorTarget iid)
            3
            [Spell]
            (ShuffleBackIn $ DrawFound iid)
        ]
      )
    _ -> ArcaneInitiate <$> runMessage msg attrs
