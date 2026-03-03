#!/bin/zsh
# codebuddy-input.sh - VTerm 输入循环脚本
# 通过 vterm escape sequence 将用户输入传回 Emacs
# 需要终端使用 Nerd Font 字体以正确显示图标

# 颜色定义
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
DIM='\033[2m'
BOLD='\033[1m'
RESET='\033[0m'

# Nerd Font 图标 (需要终端使用 Nerd Font)
ICON_ROBOT=$'\uf544'       # 󰖙 nf-md-robot
ICON_USER=$'\uf007'        #  nf-fa-user
ICON_CHAT=$'\uf4ad'        #  nf-oct-comment_discussion
ICON_SEND=$'\uf1d8'        #  nf-fa-paper_plane
ICON_CHECK=$'\uf00c'       #  nf-fa-check
ICON_FOLDER=$'\uf07b'      #  nf-fa-folder
ICON_HELP=$'\uf059'        #  nf-fa-question_circle
ICON_STOP=$'\uf28d'        #  nf-fa-stop_circle
ICON_BOLT=$'\uf0e7'        #  nf-fa-bolt
ICON_CLOCK=$'\uf017'       #  nf-fa-clock_o
ICON_KEYBOARD=$'\uf11c'    #  nf-fa-keyboard_o
ICON_MULTILINE=$'\uf038'   #  nf-fa-align_justify
ICON_BRAIN=$'\uf085'        #  nf-fa-cogs (思考/处理)

# 通过 vterm escape sequence 发送命令给 Emacs
vterm_cmd() {
    if [ -n "$TMUX" ] && { [ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]; }; then
        printf "\ePtmux;\e\e]51;E"
    elif [ "${TERM%%-*}" = "screen" ]; then
        printf "\eP\e]51;E"
    else
        printf "\e]51;E"
    fi
    local IFS=""
    while [ $# -gt 0 ]; do
        printf '"%s" ' "$(printf '%s' "$1" | sed 's/"/\\"/g')"
        shift
    done
    if [ -n "$TMUX" ] && { [ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]; }; then
        printf "\e\\"
    elif [ "${TERM%%-*}" = "screen" ]; then
        printf "\e\\"
    else
        printf "\e\\"
    fi
}

show_help() {
    echo ""
    echo "${CYAN}${BOLD}${ICON_ROBOT} CodeBuddy Chat${RESET}"
    echo "${DIM}────────────────────────────────${RESET}"
    echo "  ${ICON_SEND} 输入问题后按 ${GREEN}Enter${RESET} 发送"
    echo "  ${ICON_MULTILINE} 多行输入：输入 ${YELLOW}<<${RESET} 进入多行模式，空行结束"
    echo "  ${ICON_HELP} 输入 ${YELLOW}/help${RESET} 查看所有指令"
    echo "  ${ICON_STOP} 按 ${YELLOW}Ctrl-C${RESET} 中断执行"
    echo "${DIM}────────────────────────────────${RESET}"
    echo ""
}

# 捕获 SIGUSR1 用于 Emacs 端通知"执行完成，可以继续输入"
READY=1
handle_ready() {
    READY=1
}
trap handle_ready USR1

# 捕获 SIGINT 用于中断
handle_int() {
    vterm_cmd codebuddy--vterm-interrupt
    echo ""
    echo "${YELLOW}${ICON_STOP} 已发送中断信号${RESET}"
}
trap handle_int INT

# 初始显示帮助
show_help

# 主循环
while true; do
    # 使用 vared 读取输入（支持左右移动、行内编辑）
    # vared -p 设置 prompt，-c 创建新变量
    line=""
    vared -p $'%{\033[0;32m\033[1m%}'"${ICON_USER}"$' You> %{\033[0m%}' -c line 2>/dev/null || break

    # 空输入跳过
    [ -z "$line" ] && continue

    # 多行模式：以 << 开头
    if [ "$line" = "<<" ]; then
        echo "${DIM}${ICON_MULTILINE} (多行模式：输入内容，空行结束)${RESET}"
        input=""
        while IFS= read -r mline; do
            [ -z "$mline" ] && break
            if [ -z "$input" ]; then
                input="$mline"
            else
                input="$input
$mline"
            fi
        done
    else
        input="$line"
    fi

    # 跳过空输入
    [ -z "$input" ] && continue

    # 发送给 Emacs
    READY=0
    vterm_cmd codebuddy--vterm-send "$input"

    # 等待执行完成，显示旋转动画
    local spinner_frames=('⠋' '⠙' '⠹' '⠸' '⠼' '⠴' '⠦' '⠧' '⠇' '⠏')
    local spin_idx=0
    local start_ts=$SECONDS
    while [ "$READY" -eq 0 ]; do
        local elapsed=$((SECONDS - start_ts))
        local frame=${spinner_frames[$((spin_idx % ${#spinner_frames[@]} + 1))]}
        printf "\r${CYAN}${ICON_BRAIN} ${frame} 思考中... ${DIM}${ICON_CLOCK} ${elapsed}s${RESET}  "
        spin_idx=$((spin_idx + 1))
        sleep 0.1
    done
    local total=$((SECONDS - start_ts))
    # 清除进度行，显示完成提示
    printf "\r${GREEN}${ICON_CHECK} 完成 ${DIM}(${total}s)${RESET}                       \n"
done
