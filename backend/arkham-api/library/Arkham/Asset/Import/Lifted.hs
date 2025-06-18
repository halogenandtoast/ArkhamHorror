module Arkham.Asset.Import.Lifted (module X, module Arkham.Asset.Import.Lifted)
where

import Arkham.Asset.Runner as X (
  AssetAttrs (..),
  AssetCard,
  Field (..),
  IsAsset,
  WhenNoUses (..),
  ally,
  allyWith,
  asset,
  assetWith,
  canLeavePlayByNormalMeansL,
  cardsUnderneathL,
  controlledBy,
  controllerL,
  controls,
  discardWhenNoUses,
  driverL,
  flippedL,
  getAssetMetaDefault,
  getMetaKey,
  getMetaKeyDefault,
  handleTargetChoice,
  hasUses,
  healthL,
  is,
  metaMapL,
  noSlots,
  overMeta,
  printedUsesL,
  push,
  pushAll,
  pushM,
  pushWhen,
  pushWhenM,
  sanityL,
  sealedChaosTokensL,
  setMeta,
  setMetaKey,
  setTokens,
  slotsL,
  toMessage,
  tokensL,
  unsetMetaKey,
  visibleL,
  whenNoUsesL,
 )

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Helpers.Modifiers as X (modified, toModifiers)
import Arkham.Id as X
import Arkham.Message as X (
  Message (..),
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern SuccessfulInvestigationWith,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Token
import Control.Lens (non)

healAssetDamage :: Sourceable source => AssetAttrs -> source -> Int -> UI Message
healAssetDamage attrs source n = AssetDamageLabel attrs.id [HealDamage (toTarget attrs) (toSource source) n]

healAssetHorror :: Sourceable source => AssetAttrs -> source -> Int -> UI Message
healAssetHorror attrs source n = AssetHorrorLabel attrs.id [HealHorror (toTarget attrs) (toSource source) n]

replenish :: Token -> Int -> Tokens -> Tokens
replenish token n tokens = tokens & at token . non 0 %~ max n

replenishN :: Token -> Int -> Int -> Tokens -> Tokens
replenishN token tokenMax n tokens = tokens & at token . non 0 %~ min tokenMax . (+ n)
