module Arkham.Location.Cards.OnyxGates (onyxGates, OnyxGates (..)) where

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.Game.Helpers (perPlayer)
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype OnyxGates = OnyxGates LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onyxGates :: LocationCard OnyxGates
onyxGates = location OnyxGates Cards.onyxGates 1 (Static 12)

instance HasModifiersFor OnyxGates where
  getModifiersFor target (OnyxGates attrs) | attrs `is` target = do
    n <- perPlayer 1
    pure $ toModifiers attrs [ShroudModifier n]
  getModifiersFor _ _ = pure []

instance HasAbilities OnyxGates where
  getAbilities (OnyxGates attrs) =
    extendRevealed
      attrs
      [mkAbility attrs 1 $ forced $ RevealLocation #after Anyone $ be attrs]

instance RunMessage OnyxGates where
  runMessage msg l@(OnyxGates attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getRecordCount EvidenceOfKadath
      when (n > 0) (removeTokens (attrs.ability 1) attrs #clue n)
      pure l
    _ -> OnyxGates <$> lift (runMessage msg attrs)
