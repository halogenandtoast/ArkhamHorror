module Arkham.Asset.Assets.OlivierBishopHaughtyArtCollector (olivierBishopHaughtyArtCollector) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype OlivierBishopHaughtyArtCollector = OlivierBishopHaughtyArtCollector AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

olivierBishopHaughtyArtCollector :: AssetCard OlivierBishopHaughtyArtCollector
olivierBishopHaughtyArtCollector = asset OlivierBishopHaughtyArtCollector Cards.olivierBishopHaughtyArtCollector

instance HasModifiersFor OlivierBishopHaughtyArtCollector where
  getModifiersFor (OlivierBishopHaughtyArtCollector a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities OlivierBishopHaughtyArtCollector where
  getAbilities (OlivierBishopHaughtyArtCollector a) =
    [ controlled
        a
        1
        ( DuringTurn You
            <> youExist (InvestigatorCanMoveTo (a.ability 1) $ ConnectedFrom ForMovement YourLocation)
        )
        $ freeTrigger (exhaust a)
    ]

instance RunMessage OlivierBishopHaughtyArtCollector where
  runMessage msg a@(OlivierBishopHaughtyArtCollector attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseOrRunOneM iid $ targets locations $ moveTo (attrs.ability 1) iid

      pure a
    _ -> OlivierBishopHaughtyArtCollector <$> liftRunMessage msg attrs
