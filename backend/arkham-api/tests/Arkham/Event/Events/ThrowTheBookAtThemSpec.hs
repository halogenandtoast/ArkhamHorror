module Arkham.Event.Events.ThrowTheBookAtThemSpec (spec) where

import Arkham.Ability.Types (Ability (..), abilitySource)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

{- | Regression coverage for issue #5801.

"Throw the Book at Them!" resolves an [action] or [fast] ability on a [Tome]
asset you control. True Magick: Reworking Reality (5) IS a [Tome], and the FAQ
(February 2025) is explicit that this is a legal target: "if you successfully
fight and choose to resolve an ability 'on' True Magick, you may then treat True
Magick as a revealed Spell asset in your hand."

The card published its chosen ability as @AbilityLabel iid ab [] [] []@ -- with
no windows. True Magick's wrapper re-filters its in-hand spells with
@getCanPerformAbility iid ws@, which guards @notNull matching@ ahead of any
criteria, so an empty window list rejected every spell and @chooseOne@ threw on
the empty list. The player saw the card do nothing.
-}
spec :: Spec
spec = describe "Throw the Book at Them!" $ do
  context "with True Magick (5) as the chosen Tome (issue #5801)" $ do
    it "offers True Magick's borrowed in-hand Spell ability after the attack" . gameTest $ \self -> do
      withProp @"combat" 5 self
      withProp @"resources" 5 self
      -- horror to heal, so the borrowed Clarity of Mind [action] is performable
      withProp @"horror" 5 self
      location <- testLocation
      enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
      setChaosTokens [Zero]
      enemy `spawnAt` location
      self `moveTo` location

      trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
      inHandSpell <- self `genMyCard` Assets.clarityOfMind
      addToHand self inHandSpell

      throwTheBook <- genCard Events.throwTheBookAtThem
      self `addToHand` throwTheBook

      duringTurn self do
        self `playCard` throwTheBook
        -- True Magick is the only [Tome] in play
        chooseTarget trueMagick
        chooseTarget enemy
        startSkillTest
        applyResults
        clickLabel "$cards.label.throwTheBookAtThem.resolveAbilityOnTome"

        -- True Magick's wrapper, the single entry: the ProxySource re-sourcings
        -- getTrueMagickInHandAbilities surfaces are dropped because only the
        -- wrapper reveals the card from hand.
        chooseOptionMatching "True Magick's ability" $ \case
          AbilityLabel {ability} ->
            abilitySource ability == AssetSource trueMagick
              && ability.abilityCardCode == toCardCode Assets.trueMagickReworkingReality5
          _ -> False

        -- pre-fix: `chooseOne` threw here, because the wrapper filtered its hand
        -- against an empty window list and found nothing to offer
        chooseTarget (toCardId inHandSpell)
        chooseOnlyOption "resolve the borrowed Clarity of Mind [action]"
