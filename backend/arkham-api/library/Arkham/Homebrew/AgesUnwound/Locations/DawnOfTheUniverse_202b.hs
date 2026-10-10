module Arkham.Homebrew.AgesUnwound.Locations.DawnOfTheUniverse_202b (
  dawnOfTheUniverse_202b,
) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetStartingUses, AssetUses))
import Arkham.Helpers.Use (toStartingUses)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (timeRunsOutI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DawnOfTheUniverse_202b = DawnOfTheUniverse_202b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Revelation__ - Put Dawn of the Universe into play." The reverse of
@:ages-unwound:202@; act 4 flips it up and placing it is resolving it.
-}
dawnOfTheUniverse_202b :: LocationCard DawnOfTheUniverse_202b
dawnOfTheUniverse_202b =
  symbolLabel
    $ locationWith DawnOfTheUniverse_202b Cards.dawnOfTheUniverse_202b 6 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "__Forced__ - After you fail a skill test while investigating the Dawn of the
Universe: Return an asset you control to your hand. If that asset has uses, take X
damage or X horror, where X is the difference between the listed number of uses
and the number of uses remaining on the card. /
__Forced__ - At the end of the round, if there are any clues on this location:
Place 1 doom on the current agenda."
-}
instance HasAbilities DawnOfTheUniverse_202b where
  getAbilities (DawnOfTheUniverse_202b a) =
    extendRevealed
      a
      [ restricted a 1 (exists $ You <> HasMatchingAsset (AssetControlledBy You))
          $ forced
          $ SkillTestResult #after You (WhileInvestigating (be a)) #failure
      , restricted a 2 (thisExists a LocationWithAnyClues) $ forced $ RoundEnds #when
      ]

instance RunMessage DawnOfTheUniverse_202b where
  runMessage msg l@(DawnOfTheUniverse_202b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid
      chooseTargetM iid assets \aid -> do
        {- "the difference between the listed number of uses and the number of
        uses remaining" -- spent uses, which is why the printed total has to be
        recomputed rather than read off the entity. An asset with no uses gives
        0, and the choice is skipped. -}
        starting <- fieldMapM AssetStartingUses toStartingUses aid
        remaining <- field AssetUses aid
        let spent = sum starting - sum remaining
        returnToHand iid aid
        when (spent > 0) do
          chooseOneM iid $ timeRunsOutI18n $ scope "dawnOfTheUniverse" do
            countVar spent $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) spent
            countVar spent $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) spent
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeDoomOnAgenda 1
      pure l
    _ -> DawnOfTheUniverse_202b <$> liftRunMessage msg attrs
