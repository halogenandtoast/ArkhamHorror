module Arkham.Asset.Cards.IdolOfXanatosWatcherBeyondTime (
  idolOfXanatosWatcherBeyondTime,
  IdolOfXanatosWatcherBeyondTime (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (dealtDamage, dealtHorror)
import Arkham.Matcher
import Arkham.Window qualified as W

newtype IdolOfXanatosWatcherBeyondTime = IdolOfXanatosWatcherBeyondTime AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities IdolOfXanatosWatcherBeyondTime where
  getAbilities (IdolOfXanatosWatcherBeyondTime a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (DealtDamageOrHorror #when (SourceIsCancelable AnySource) You)
          (exhaust a <> UpTo (Fixed 3) (HandDiscardCost 1 AnyCard))
    ]

idolOfXanatosWatcherBeyondTime :: AssetCard IdolOfXanatosWatcherBeyondTime
idolOfXanatosWatcherBeyondTime = asset IdolOfXanatosWatcherBeyondTime Cards.idolOfXanatosWatcherBeyondTime

instance RunMessage IdolOfXanatosWatcherBeyondTime where
  runMessage msg a@(IdolOfXanatosWatcherBeyondTime attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws (totalDiscardCardPayments -> n) -> do
      when (n > 0) do
        let (damage, horror) = (dealtDamage ws, dealtHorror ws)
        if damage > 0 && horror > 0
          then
            chooseN iid (min n $ damage + horror)
              $ (replicate damage $ Label "Cancel 1 damage" [CancelDamage iid 1])
              <> (replicate horror $ Label "Cancel 1 horror" [CancelHorror iid 1])
          else
            pushAll
              $ [CancelDamage iid (min n damage) | damage > 0]
              <> [CancelHorror iid (min n horror) | horror > 0]
        checkAfter $ W.CancelledOrIgnoredCardOrGameEffect $ attrs.ability 1
      pure a
    _ -> IdolOfXanatosWatcherBeyondTime <$> liftRunMessage msg attrs
