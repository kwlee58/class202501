## ---------------------------------------------------------------------------
## Red and Black : 최적 seed 탐색 (고속판)
##
## 배정 방식은 기존 Red_and_Black_prototype_v5.Rmd 와 완전히 동일합니다.
##   set.seed(k); sample.int(N) %% 2  ->  0 = Red, 1 = Black
## 따라서 같은 seed 는 기존 코드와 같은 Red/Black 배치를 만듭니다.
##
## 달라진 것은 카이제곱 계산 방법뿐입니다.
##   chisq.test() 를 5번 호출하는 대신, 2 x k 분할표의 카이제곱을
##   닫힌 식으로 직접 계산합니다.  결과값은 소수점까지 동일합니다.
##      X^2 = (N^2 / (n1*n2)) * sum_j (a_j - n1*c_j/N)^2 / c_j
##   (a_j = Red 중 j 범주 인원, c_j = 전체 j 범주 인원)
##
## 벤치마크(618명, 1코어): 기존 1,127us/회 -> 77us/회 = 약 15배
##   1,000만 회 : 약 3.1시간 -> 약 0.2시간 (1코어), 멀티코어면 수 분
## ---------------------------------------------------------------------------

library(readxl)
library(parallel)

## ===== 설정 ================================================================
ROLL_FILE <- "./data/class_roll_260907.xlsx"   # 이번 학기 출석부
OUT_FILE  <- "./data/rb_search_260907.RDS"     # 결과 저장
N_TOTAL   <- 1e7                               # 총 시행 횟수
CHUNK     <- 1e5                               # 코어에 던지는 덩어리
CORES     <- max(1L, detectCores() - 1L)
SEED_FROM <- 1                                 # 탐색 시작 seed

## ===== 1. 출석부 ===========================================================
## LMS 출석부는 A열에 순번이 붙어 나오고 자료는 B열부터 시작한다.
## 예전 코드의 range = "B1:H618" 은 인원이 바뀌면 매번 고쳐야 했다.
## 여기서는 머리글 이름으로 열을 찾는다. 인원도 열 순서도 바뀔 수 있기 때문이다.
## 찾은 결과를 화면에 찍으니 탐색을 시작하기 전에 반드시 눈으로 확인할 것.

raw <- read_excel(ROLL_FILE, .name_repair = "minimal")

PAT <- c(dept    = "학과|학부|전공",
         college = "단과|대학",
         id      = "학번",
         name    = "이름|성명",
         status  = "학적|상태|구분",
         email   = "메일|mail",
         cell_no = "휴대|전화|연락처|핸드폰")

hdr  <- names(raw)
pick <- vapply(PAT, function(p) {
  i <- grep(p, hdr, ignore.case = TRUE)
  if (length(i) == 0) NA_integer_ else i[1]
}, 1L)

if (anyNA(pick)) {
  cat("머리글로 찾지 못한 항목:", paste(names(pick)[is.na(pick)], collapse = ", "), "\n")
  cat("엑셀 머리글:", paste(hdr, collapse = " | "), "\n")
  cat("-> 예전 방식대로 B~H열 순서로 읽습니다. 아래 대응표를 꼭 확인하십시오.\n")
  stopifnot(ncol(raw) >= 8)
  class_roll <- raw[, 2:8]
  used <- hdr[2:8]
} else {
  class_roll <- raw[, pick]
  used <- hdr[pick]
}
names(class_roll) <- c("dept", "college", "id", "name", "status", "email", "cell_no")

cat("\n[열 대응 확인]\n")
print(data.frame(변수 = names(class_roll), 엑셀머리글 = used,
                 첫값 = sapply(class_roll, function(x) as.character(x)[1]),
                 row.names = NULL))
cat("\n")

class_roll$id <- as.character(class_roll$id)
class_roll <- class_roll[!is.na(class_roll$id) & class_roll$id != "", ]
N <- nrow(class_roll)
cat(sprintf("출석부 %d명\n", N))

## ===== 2. 다섯 개 범주 변수 ================================================
## 기존과 같은 정의. 전화번호만 형식에 덜 민감하게 바꿨습니다(아래 주석 참조).

## (1) 학번 : 2020년 이전은 하나로 묶음
yr <- suppressWarnings(as.numeric(substr(class_roll$id, 1, 4)))
f_id <- factor(ifelse(yr <= 2020, "2020이전", as.character(yr)),
               levels = c("2020이전", sort(unique(as.character(yr[yr > 2020])))))

## (2) e-mail 서비스업체
isp <- class_roll$email |>
  strsplit("@", fixed = TRUE) |> sapply("[", 2) |>
  strsplit(".", fixed = TRUE)  |> sapply("[", 1) |> tolower()
f_email <- factor(ifelse(isp %in% c("naver", "gmail"), isp, "기타서비스"),
                  levels = c("naver", "gmail", "기타서비스"))

## (3) 전화번호 끝 네 자리의 천 단위
##   기존 substr(8, 11) 은 "010-1234-5678" 처럼 하이픈이 있으면 자리가 밀리고,
##   끝 네 자리가 "0000" 인 사람은 cut() 에서 NA 가 되어 표에서 빠졌습니다.
##   숫자만 남긴 뒤 끝 네 자리를 쓰고, 0000 도 0000~0999 구간에 넣습니다.
digits <- gsub("[^0-9]", "", class_roll$cell_no)
tail4  <- suppressWarnings(as.numeric(substr(digits, nchar(digits) - 3, nchar(digits))))
f_phone <- factor(pmin(tail4 %/% 1000, 9), levels = 0:9,
                  labels = paste(paste0(0:9, "000"), paste0(0:9, "999"), sep = "~"))

## (4) 성씨
f1 <- substring(class_roll$name, 1, 1)
f_name <- factor(ifelse(f1 %in% c("김", "이", "박", "최", "정"), f1, "기타"),
                 levels = c("김", "이", "박", "최", "정", "기타"))

## (5) 단과대학
f_college <- factor(class_roll$college)

VARS <- list(학번 = f_id, email = f_email, 전화번호 = f_phone,
             성씨 = f_name, 단과대학 = f_college)

## 결측 확인 — 여기서 걸러내야 나중에 표가 조용히 어긋나지 않습니다.
na_count <- sapply(VARS, function(v) sum(is.na(v)))
if (any(na_count > 0)) {
  cat("주의: 결측이 있는 변수\n"); print(na_count[na_count > 0])
}

## 탐색은 몇 분~몇십 분 걸린다. 시작 전에 다섯 변수의 분포를 눈으로 본다.
cat("\n[다섯 변수 분포]\n")
for (nm in names(VARS)) {
  cat("--", nm, "\n"); print(table(VARS[[nm]], useNA = "ifany"))
}
cat("\n")

## ===== 3. 탐색 준비 ========================================================
codes <- lapply(VARS, as.integer)                    # 범주 -> 정수 코드
K     <- vapply(VARS, nlevels, 1L)                   # 범주 수
CT    <- Map(function(z, k) tabulate(z, nbins = k), codes, K)   # 열 합계
DF    <- K - 1L                                      # 변수별 자유도
n1    <- sum(seq_len(N) %% 2 == 0)                   # Red 인원
n2    <- N - n1
KONST <- N^2 / (n1 * n2)

cat(sprintf("변수별 자유도: %s  (합계 %d)\n",
            paste(names(DF), DF, sep = "=", collapse = ", "), sum(DF)))

## 한 seed 의 Xsum
xsum_one <- function(k) {
  set.seed(k)
  red <- which(sample.int(N) %% 2 == 0)
  s <- 0
  for (j in seq_along(codes)) {
    a  <- tabulate(codes[[j]][red], nbins = K[j])
    ct <- CT[[j]]
    s  <- s + KONST * sum((a - n1 * ct / N)^2 / ct)
  }
  s
}

## 변수별로 쪼개서 보고 싶을 때
xvec_one <- function(k) {
  set.seed(k)
  red <- which(sample.int(N) %% 2 == 0)
  vapply(seq_along(codes), function(j) {
    a  <- tabulate(codes[[j]][red], nbins = K[j])
    ct <- CT[[j]]
    KONST * sum((a - n1 * ct / N)^2 / ct)
  }, 0)
}

## 덩어리 하나를 처리하고 "그 안의 최소값 하나"만 돌려줍니다.
## 1,000만 개를 통째로 메모리에 담지 않으므로 파일을 10개로 쪼갤 필요가 없습니다.
best_in_chunk <- function(from) {
  ks <- from:min(from + CHUNK - 1, SEED_FROM + N_TOTAL - 1)
  v  <- vapply(ks, xsum_one, 0)
  i  <- which.min(v)
  c(seed = ks[i], Xsum = v[i])
}

## ===== 4. 탐색 =============================================================
starts <- seq(SEED_FROM, SEED_FROM + N_TOTAL - 1, by = CHUNK)
cat(sprintf("탐색 시작: seed %s ~ %s, %d 덩어리, %d 코어\n",
            format(SEED_FROM, big.mark = ","),
            format(SEED_FROM + N_TOTAL - 1, big.mark = ","),
            length(starts), CORES))

t0 <- Sys.time()
res <- mclapply(starts, best_in_chunk, mc.cores = CORES)
res <- do.call(rbind, res)
cat(sprintf("소요 시간: %.1f분\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

best      <- res[which.min(res[, "Xsum"]), ]
best_seed <- as.integer(best[["seed"]])
best_x    <- xvec_one(best_seed)

cat(sprintf("\n최적 seed = %s,  Xsum = %.4f\n",
            format(best_seed, big.mark = ","), best[["Xsum"]]))
print(data.frame(변수 = names(VARS), df = DF, X2 = round(best_x, 3),
                 p = round(1 - pchisq(best_x, DF), 4), row.names = NULL))
cat(sprintf("합계: X2 = %.3f, df = %d, p = %.4f\n",
            sum(best_x), sum(DF), 1 - pchisq(sum(best_x), sum(DF))))

## ===== 5. 검산 : 기존 방식(chisq.test)과 일치하는지 ========================
set.seed(best_seed)
grp <- factor(sample.int(N) %% 2, levels = c(0, 1), labels = c("Red", "Black"))
old_x <- vapply(VARS, function(v)
  unname(chisq.test(table(grp, v))$statistic), 0)
cat(sprintf("\n검산 (chisq.test 와의 최대 오차): %.3e\n",
            max(abs(old_x - best_x))))

## ===== 6. 저장 =============================================================
class_roll$group <- grp
saveRDS(list(seed = best_seed, Xsum = sum(best_x), X = best_x, df = DF,
             class_roll = class_roll,
             search = res, n_total = N_TOTAL, roll_file = ROLL_FILE,
             run_at = Sys.time()),
        file = OUT_FILE)
cat(sprintf("저장: %s\n", OUT_FILE))

## 탐색 분포를 보고 싶을 때 (덩어리별 최소값이 아니라 전체 분포가 필요하면
## best_in_chunk 가 v 를 통째로 돌려주도록 바꾸십시오)
## hist(res[, "Xsum"], nclass = 50)
