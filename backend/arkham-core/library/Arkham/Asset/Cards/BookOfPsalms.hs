module Arkham.Asset.Cards.BookOfPsalms (bookOfPsalms, BookOfPsalms (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Prelude

newtype BookOfPsalms = BookOfPsalms AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfPsalms :: AssetCard BookOfPsalms
bookOfPsalms =
  asset BookOfPsalms Cards.bookOfPsalms

instance HasAbilities BookOfPsalms where
  getAbilities (BookOfPsalms x) =
    [ controlledAbility
        x
        1
        ( oneOf
            [ exists (HealableInvestigator (toAbilitySource x 1) #horror $ InvestigatorAt YourLocation)
            , HasRemainingBlessTokens
            ]
        )
        (actionAbilityWithCost $ assetUseCost x Secret 1)
    ]

instance RunMessage BookOfPsalms where
  runMessage msg a@(BookOfPsalms attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iidsWithHeal <- getInvestigatorsWithHealHorror attrs 1 (colocatedWith iid)
      player <- getPlayer iid
      n <- min 2 <$> getRemainingBlessTokens
      pushAll
        $ (chooseOrRunOne player $ map (uncurry targetLabel . second only) iidsWithHeal)
        : replicate n (AddChaosToken #bless)
      pure a
    _ -> BookOfPsalms <$> runMessage msg attrs
