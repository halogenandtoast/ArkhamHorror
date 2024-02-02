module Arkham.Event.Cards.Recharge4 (
  recharge4,
  Recharge4 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy
import Arkham.Trait hiding (Cultist)
import Arkham.Window qualified as Window

newtype Meta = Meta {chosenAsset :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype Recharge4 = Recharge4 (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

recharge4 :: EventCard Recharge4
recharge4 = event (Recharge4 . (`With` Meta Nothing)) Cards.recharge4

instance RunMessage Recharge4 where
  runMessage msg e@(Recharge4 (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      assets <-
        selectTargets
          $ AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> oneOf [AssetWithTrait Spell, AssetWithTrait Relic]
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel target [ResolveEvent iid eid (Just target) windows']
          | target <- assets
          ]
      pure e
    ResolveEvent iid eid (Just (AssetTarget aid)) _ | eid == toId attrs -> do
      pushAll [RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside]
      pure $ Recharge4 $ attrs `with` Meta (Just aid)
    RequestedChaosTokens source _ tokens | isSource attrs source -> do
      push $ ResetChaosTokens (toSource attrs)
      case chosenAsset meta of
        Nothing -> error "invalid use"
        Just aid -> do
          if any
            ( (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
                . chaosTokenFace
            )
            tokens
            then
              push
                $ If
                  ( Window.RevealChaosTokenEventEffect
                      (eventOwner attrs)
                      tokens
                      (toId attrs)
                  )
                  [AddUses aid Charge 1]
            else push (AddUses aid Charge 4)
          pure e
    _ -> Recharge4 . (`with` meta) <$> runMessage msg attrs
