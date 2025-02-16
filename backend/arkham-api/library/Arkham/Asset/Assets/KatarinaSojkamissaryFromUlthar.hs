module Arkham.Asset.Assets.KatarinaSojkamissaryFromUlthar (katarinaSojkamissaryFromUlthar) where

import Arkham.ChaosBagStepState
import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Helpers.Window (getDrawSource)

newtype KatarinaSojkamissaryFromUlthar = KatarinaSojkamissaryFromUlthar AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

katarinaSojkamissaryFromUlthar :: AssetCard KatarinaSojkamissaryFromUlthar
katarinaSojkamissaryFromUlthar = ally KatarinaSojkamissaryFromUlthar Cards.katarinaSojkamissaryFromUlthar (1, 2)

instance HasAbilities KatarinaSojkamissaryFromUlthar where
  getAbilities (KatarinaSojkamissaryFromUlthar a) =
    [ controlled a 1 (DuringSkillTest $ YourSkillTest AnySkillTest)
        $ triggered (WouldRevealChaosToken #when You) (exhaust a)
    ]

instance RunMessage KatarinaSojkamissaryFromUlthar where
  runMessage msg a@(KatarinaSojkamissaryFromUlthar attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (toSource attrs) 1 ResolveChoice [Undecided (DrawUntil IsSymbol)] [] Nothing
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> KatarinaSojkamissaryFromUlthar <$> liftRunMessage msg attrs
