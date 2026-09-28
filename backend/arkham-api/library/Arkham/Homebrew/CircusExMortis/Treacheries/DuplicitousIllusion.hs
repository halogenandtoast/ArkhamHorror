module Arkham.Homebrew.CircusExMortis.Treacheries.DuplicitousIllusion (duplicitousIllusion) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken, sealMoonTokenOn)
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (windowType))
import Arkham.Window qualified as Window

newtype DuplicitousIllusion = DuplicitousIllusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duplicitousIllusion :: TreacheryCard DuplicitousIllusion
duplicitousIllusion = treachery DuplicitousIllusion Cards.duplicitousIllusion

instance HasAbilities DuplicitousIllusion where
  getAbilities (DuplicitousIllusion a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ ActivateAbility #when You (AbilityIsActionAbility <> AssetAbility (AssetControlledBy You))
    , restricted a 2 (InThreatAreaOf You <> exists moonToken) actionAbility
    ]

-- "that asset" is the one whose ability was activated, read out of the window
activatedAsset :: [Window] -> Maybe AssetId
activatedAsset ws =
  listToMaybe
    [ aid
    | Window.ActivateAbility _ _ ability <- map windowType ws
    , Just aid <- [(abilitySource ability).asset]
    ]

-- the reveal is answered on the same proxy source it was requested with
proxiedAsset :: TreacheryAttrs -> Source -> Maybe AssetId
proxiedAsset attrs = \case
  ProxySource src (AssetSource aid) | isAbilitySource attrs 1 src -> Just aid
  _ -> Nothing

doomFaces :: [ChaosTokenFace]
doomFaces = [Skull, Cultist, Tablet, ElderThing, AutoFail, MoonToken]

instance RunMessage DuplicitousIllusion where
  runMessage msg t@(DuplicitousIllusion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (activatedAsset -> Just aid) _ -> do
      -- a bare reveal, not a skill test draw: ResolveChaosToken never runs, so a
      -- revealed {moon} does not seal itself here
      requestChaosTokens iid (ProxySource (attrs.ability 1) (AssetSource aid)) 1
      pure t
    RequestedChaosTokens (proxiedAsset attrs -> Just aid) (Just iid) tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      continue iid $ when (any (`elem` doomFaces) faces) $ placeDoom (attrs.ability 1) aid 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sealMoonTokenOn iid
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> DuplicitousIllusion <$> liftRunMessage msg attrs
