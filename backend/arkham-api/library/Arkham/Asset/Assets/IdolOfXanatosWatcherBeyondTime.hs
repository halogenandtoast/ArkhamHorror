module Arkham.Asset.Assets.IdolOfXanatosWatcherBeyondTime (idolOfXanatosWatcherBeyondTime) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (dealtDamage, dealtHorror)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window qualified as W

newtype IdolOfXanatosWatcherBeyondTime = IdolOfXanatosWatcherBeyondTime AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities IdolOfXanatosWatcherBeyondTime where
  getAbilities (IdolOfXanatosWatcherBeyondTime a) =
    [ controlled_ a 1
        $ triggered
          (DealtDamageOrHorror #when (SourceIsCancelable AnySource) You)
          (exhaust a <> UpTo (Fixed 3) (HandDiscardCost 1 #any))
    ]

idolOfXanatosWatcherBeyondTime :: AssetCard IdolOfXanatosWatcherBeyondTime
idolOfXanatosWatcherBeyondTime = asset IdolOfXanatosWatcherBeyondTime Cards.idolOfXanatosWatcherBeyondTime

instance RunMessage IdolOfXanatosWatcherBeyondTime where
  runMessage msg a@(IdolOfXanatosWatcherBeyondTime attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 ws (totalDiscardCardPayments -> n) -> do
      let (damage, horror) = (dealtDamage ws, dealtHorror ws)
      when (n > 0) $ doStep n (DoStep damage (DoStep horror msg))
      pure a
    DoStep n (DoStep damage (DoStep horror msg'@(UseThisAbility iid (isSource attrs -> True) 1))) -> do
      when (n > 0 && damage + horror > 0) do
        if damage > 0 && horror > 0
          then chooseOneM iid do
            labeled "Cancel 1 damage" do
              cancelInvestigatorDamage iid 1
              doStep (n - 1) (DoStep (damage - 1) (DoStep horror msg'))
            labeled "Cancel 1 horror" do
              cancelInvestigatorHorror iid 1
              doStep (n - 1) (DoStep damage (DoStep (horror - 1) msg'))
          else do
            cancelInvestigatorDamage iid (min n damage)
            cancelInvestigatorHorror iid (min n horror)
        checkAfter $ W.CancelledOrIgnoredCardOrGameEffect (attrs.ability 1) Nothing
      pure a
    _ -> IdolOfXanatosWatcherBeyondTime <$> liftRunMessage msg attrs
