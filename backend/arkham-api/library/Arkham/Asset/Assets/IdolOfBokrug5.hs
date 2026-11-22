module Arkham.Asset.Assets.IdolOfBokrug5 (idolOfBokrug5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Dreaming (getDreamSourcesFor)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype IdolOfBokrug5 = IdolOfBokrug5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

idolOfBokrug5 :: AssetCard IdolOfBokrug5
idolOfBokrug5 = asset IdolOfBokrug5 Cards.idolOfBokrug5

{- | "(Limit 5 dreams on Idol of Bokrug.)" The only copy of the number: every
reader gets it back off 'IsDreamEndpoint', including the Forced ability's own
criterion.
-}
dreamCapacity :: Int
dreamCapacity = 5

{- | "Dreams may be moved to and from Idol of Bokrug as if it were a dreaming
card under your control." Scoped to dream MOVEMENT only: the Idol does not count
against the limit of 3 and is not "a dreaming card you control" for any other
text, which is why this is a modifier on the asset rather than a dream-zone
record.
-}
instance HasModifiersFor IdolOfBokrug5 where
  getModifiersFor (IdolOfBokrug5 a) = modifySelf a [IsDreamEndpoint (Just dreamCapacity)]

instance HasAbilities IdolOfBokrug5 where
  getAbilities (IdolOfBokrug5 a) =
    [ restricted a 1 (ControlsThis <> CanMoveADreamTo (toTarget a) You) $ forced $ TurnBegins #after You
    , restricted a 2 (ControlsThis <> holdsADream <> exists secretsAsset) actionAbility
    ]
   where
    holdsADream = exists (be a <> AssetWithTokens (atLeast 1) Dream)
    secretsAsset = AssetControlledBy You <> AssetWithUseType Secret

instance RunMessage IdolOfBokrug5 where
  runMessage msg a@(IdolOfBokrug5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      {- Every dream place the investigator controls except this one: the Idol
      cannot be its own source (that is not a move), but another card made a
      dream place the same way is a legal source. Same predicate the criterion
      above is derived from, so the gate and the list cannot disagree. -}
      sources <- getDreamSourcesFor (toTarget attrs) iid
      -- one MoveTokens bridges both ends, so the last dream coming off a
      -- dreaming card flips it faceup at once
      chooseOrRunOneM iid $ targets sources \from -> moveTokens (attrs.ability 1) from attrs Dream 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let dreams = countTokens Dream attrs.tokens
      assets <- select $ assetControlledBy iid <> AssetWithUseType Secret
      -- "as secrets" is a conversion, which one MoveTokens cannot carry: it
      -- names a single token type for both ends
      chooseOrRunOneM iid $ targets assets \aid -> do
        removeTokens (attrs.ability 2) attrs Dream dreams
        placeTokens (attrs.ability 2) aid Secret dreams
      pure a
    _ -> IdolOfBokrug5 <$> liftRunMessage msg attrs
